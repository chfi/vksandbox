// adapted from https://github.com/vulkano-rs/vulkano/blob/master/examples/src/bin/triangle.rs

use vulkano::buffer::{BufferUsage, CpuAccessibleBuffer};
use vulkano::command_buffer::{AutoCommandBufferBuilder, DynamicState, SubpassContents};
use vulkano::device::{Device, DeviceExtensions};
use vulkano::framebuffer::{Framebuffer, FramebufferAbstract, RenderPassAbstract, Subpass};
use vulkano::image::{ImageUsage, SwapchainImage};
use vulkano::instance::{Instance, PhysicalDevice};

use vulkano::pipeline::{viewport::Viewport, GraphicsPipeline};

use vulkano::swapchain::{
    self, AcquireError, ColorSpace, FullscreenExclusive, PresentMode, SurfaceTransform, Swapchain,
    SwapchainCreationError,
};
use vulkano::sync::{self, FlushError, GpuFuture};

use vulkano_win::VkSurfaceBuild;
use winit::event::{Event, WindowEvent};
use winit::event_loop::{ControlFlow, EventLoop};
use winit::window::{Window, WindowBuilder};

use std::sync::Arc;

#[derive(Default, Debug, Clone)]
struct Vertex {
    position: [f32; 2],
}
vulkano::impl_vertex!(Vertex, position);

fn circular_motion(origin: [f32; 2], radius: f32, f_mult: f32, t: f32) -> [f32; 2] {
    let [o_x, o_y] = origin;
    let t_ = t * f_mult;
    let x = (radius * t_.sin()) + o_x;
    let y = (radius * t_.cos()) + o_y;
    [x, y]
}

fn vertices(t: f32) -> [Vertex; 3] {
    let v_0 = circular_motion([-0.5, -0.25], 0.5, 1.0, t);
    let v_1 = circular_motion([0.0, 0.5], 0.3, 2.0, t);
    let v_2 = circular_motion([0.25, 0.1], 0.2, 3.0, t);

    [
        Vertex { position: v_0 },
        Vertex { position: v_1 },
        Vertex { position: v_2 },
    ]
}

fn main() {
    let required_extensions = vulkano_win::required_extensions();

    let instance = Instance::new(None, &required_extensions, None).unwrap();

    let physical = PhysicalDevice::enumerate(&instance).next().unwrap();

    // Some little debug infos.
    println!(
        "Using device: {} (type: {:?})",
        physical.name(),
        physical.ty()
    );

    let event_loop = EventLoop::new();
    let surface = WindowBuilder::new()
        .build_vk_surface(&event_loop, instance.clone())
        .unwrap();

    let queue_family = physical
        .queue_families()
        .find(|&q| q.supports_graphics() && surface.is_supported(q).unwrap_or(false))
        .unwrap();

    let device_ext = DeviceExtensions {
        khr_swapchain: true,
        ..DeviceExtensions::none()
    };
    let (device, mut queues) = Device::new(
        physical,
        physical.supported_features(),
        &device_ext,
        [(queue_family, 0.5)].iter().cloned(),
    )
    .unwrap();

    let queue = queues.next().unwrap();

    let (mut swapchain, images) = {
        let caps = surface.capabilities(physical).unwrap();

        let alpha = caps.supported_composite_alpha.iter().next().unwrap();

        let format = caps.supported_formats[0].0;

        let dimensions: [u32; 2] = surface.window().inner_size().into();

        Swapchain::new(
            device.clone(),
            surface.clone(),
            caps.min_image_count,
            format,
            dimensions,
            1,
            ImageUsage::color_attachment(),
            &queue,
            SurfaceTransform::Identity,
            alpha,
            PresentMode::Fifo,
            FullscreenExclusive::Default,
            true,
            ColorSpace::SrgbNonLinear,
        )
        .unwrap()
    };

    let vertex_buffer: Arc<CpuAccessibleBuffer<[Vertex]>> = {
        CpuAccessibleBuffer::from_iter(
            device.clone(),
            BufferUsage::all(),
            false,
            vertices(0.0).iter().cloned(),
        )
        .unwrap()
    };

    let _vs = include_str!("../glsl/vertex.vert");

    mod vs {
        vulkano_shaders::shader! {
            ty: "vertex",
            path: "glsl/vertex.vert",
        }
    }
    let _fs = include_str!("../glsl/fragment.frag");

    mod fs {
        vulkano_shaders::shader! {
            ty: "fragment",
            path: "glsl/fragment.frag",
        }
    }

    let _point_fs = include_str!("../glsl/point.frag");

    mod point_fs {
        vulkano_shaders::shader! {
            ty: "fragment",
            path: "glsl/point.frag",
        }
    }

    let _gs = include_str!("../glsl/geometry.geom");

    mod gs {
        vulkano_shaders::shader! {
            ty: "geometry",
            path: "glsl/geometry.geom",
        }
    }

    let vs = vs::Shader::load(device.clone()).unwrap();

    let fs = fs::Shader::load(device.clone()).unwrap();
    let point_fs = point_fs::Shader::load(device.clone()).unwrap();

    let gs = gs::Shader::load(device.clone()).unwrap();

    let render_pass = Arc::new(
        vulkano::single_pass_renderpass!(
            device.clone(),
            attachments: {
                color: {
                    load: Clear,
                    store: Store,
                    format: swapchain.format(),
                    samples: 1,
                }
            },
            pass: {
                color: [color],
                depth_stencil: {}
            }
        )
        .unwrap(),
    );

    let pipeline = Arc::new(
        GraphicsPipeline::start()
            .vertex_input_single_buffer()
            .vertex_shader(vs.main_entry_point(), ())
            .point_list()
            // .geometry_shader(gs.main_entry_point(), ())
            // .triangle_list()
            .viewports_dynamic_scissors_irrelevant(1)
            // .fragment_shader(fs.main_entry_point(), ())
            .fragment_shader(point_fs.main_entry_point(), ())
            .render_pass(Subpass::from(render_pass.clone(), 0).unwrap())
            .blend_alpha_blending()
            .build(device.clone())
            .unwrap(),
    );

    let mut dynamic_state = DynamicState {
        line_width: None,
        viewports: None,
        scissors: None,
        compare_mask: None,
        write_mask: None,
        reference: None,
    };

    let mut framebuffers =
        window_size_dependent_setup(&images, render_pass.clone(), &mut dynamic_state);

    let mut recreate_swapchain = false;

    let mut previous_frame_end = Some(sync::now(device.clone()).boxed());

    use std::time::{Duration, Instant, SystemTime};

    let mut last_time = Instant::now();
    let mut t = 0.0;
    let mut since_last_render = 0.0;

    event_loop.run(move |event, _, control_flow| {
        let now = Instant::now();
        let delta = now.duration_since(last_time);

        t += delta.as_secs_f32();
        since_last_render += delta.as_secs_f32();

        last_time = now;

        match event {
            Event::WindowEvent {
                event: WindowEvent::CloseRequested,
                ..
            } => {
                *control_flow = ControlFlow::Exit;
            }
            Event::WindowEvent {
                event: WindowEvent::Resized(_),
                ..
            } => {
                recreate_swapchain = true;
            }
            Event::RedrawEventsCleared => {
                previous_frame_end.as_mut().unwrap().cleanup_finished();

                if recreate_swapchain {
                    let dimensions: [u32; 2] = surface.window().inner_size().into();

                    let (new_swapchain, new_images) =
                        match swapchain.recreate_with_dimensions(dimensions) {
                            Ok(r) => r,
                            Err(SwapchainCreationError::UnsupportedDimensions) => return,
                            Err(e) => panic!("Failed to recreate swapchain: {:?}", e),
                        };

                    swapchain = new_swapchain;
                    framebuffers = window_size_dependent_setup(
                        &new_images,
                        render_pass.clone(),
                        &mut dynamic_state,
                    );
                    recreate_swapchain = false;
                }
                let (image_num, suboptimal, acquire_future) =
                    match swapchain::acquire_next_image(swapchain.clone(), None) {
                        Ok(r) => r,
                        Err(AcquireError::OutOfDate) => {
                            recreate_swapchain = true;
                            return;
                        }
                        Err(e) => panic!("Failed to acquire next image: {:?}", e),
                    };

                if suboptimal {
                    recreate_swapchain = true;
                }

                let clear_values = vec![[0.0, 0.0, 0.1, 1.0].into()];

                let mut builder = AutoCommandBufferBuilder::primary_one_time_submit(
                    device.clone(),
                    queue.family(),
                )
                .unwrap();

                builder
                    .begin_render_pass(
                        framebuffers[image_num].clone(),
                        SubpassContents::Inline,
                        clear_values,
                    )
                    .unwrap()
                    .draw(
                        pipeline.clone(),
                        &dynamic_state,
                        vertex_buffer.clone(),
                        (),
                        (),
                    )
                    .unwrap()
                    .end_render_pass()
                    .unwrap();

                let command_buffer = builder.build().unwrap();

                let future = previous_frame_end
                    .take()
                    .unwrap()
                    .join(acquire_future)
                    .then_execute(queue.clone(), command_buffer)
                    .unwrap()
                    .then_swapchain_present(queue.clone(), swapchain.clone(), image_num)
                    .then_signal_fence_and_flush();

                match future {
                    Ok(future) => {
                        if let Ok(_) = future.wait(None) {
                            if let Ok(write_lock) = vertex_buffer.write() {
                                println!("delta: {}", since_last_render);

                                since_last_render = 0.0;

                                let mut buf: vulkano::buffer::cpu_access::WriteLock<[Vertex]> =
                                    write_lock;

                                let mut new_vertices = vertices(t);

                                for i in 0..=2 {
                                    std::mem::swap(&mut buf[i], &mut new_vertices[i]);
                                }
                            }
                        }

                        previous_frame_end = Some(future.boxed());
                    }
                    Err(FlushError::OutOfDate) => {
                        recreate_swapchain = true;
                        previous_frame_end = Some(sync::now(device.clone()).boxed());
                    }
                    Err(e) => {
                        println!("Failed to flush future: {:?}", e);
                        previous_frame_end = Some(sync::now(device.clone()).boxed());
                    }
                }
            }
            _ => (),
        }
    });
}

/// This method is called once during initialization, then again whenever the window is resized
fn window_size_dependent_setup(
    images: &[Arc<SwapchainImage<Window>>],
    render_pass: Arc<dyn RenderPassAbstract + Send + Sync>,
    dynamic_state: &mut DynamicState,
) -> Vec<Arc<dyn FramebufferAbstract + Send + Sync>> {
    let dimensions = images[0].dimensions();

    let viewport = Viewport {
        origin: [0.0, 0.0],
        dimensions: [dimensions[0] as f32, dimensions[1] as f32],
        depth_range: 0.0..1.0,
    };
    dynamic_state.viewports = Some(vec![viewport]);

    images
        .iter()
        .map(|image| {
            Arc::new(
                Framebuffer::start(render_pass.clone())
                    .add(image.clone())
                    .unwrap()
                    .build()
                    .unwrap(),
            ) as Arc<dyn FramebufferAbstract + Send + Sync>
        })
        .collect::<Vec<_>>()
}

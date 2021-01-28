use vulkano::descriptor::PipelineLayoutAbstract;
use vulkano::device::Device;
use vulkano::framebuffer::{RenderPassAbstract, Subpass};
use vulkano::pipeline::GraphicsPipeline;

use std::sync::Arc;

#[derive(Default, Debug, Clone, Copy)]
pub struct Vertex {
    pub position: [f32; 2],
}
vulkano::impl_vertex!(Vertex, position);

pub use circles::*;

pub mod circles {

    use super::*;

    fn _dumb() {
        let _vs = include_str!("../glsl/vertex.vert");
        let _fs = include_str!("../glsl/point.frag");
    }

    mod vs {
        vulkano_shaders::shader! {
            ty: "vertex",
            path: "glsl/vertex.vert",
        }
    }

    mod fs {
        vulkano_shaders::shader! {
            ty: "fragment",
            path: "glsl/point.frag",
        }
    }

    pub type Pipeline = GraphicsPipeline<
        vulkano::pipeline::vertex::SingleBufferDefinition<Vertex>,
        Box<dyn PipelineLayoutAbstract + Send + Sync>,
        std::sync::Arc<dyn RenderPassAbstract + Send + Sync>,
    >;

    pub struct CirclesGfx {
        vertices: Vec<Vertex>,
        pipeline: Arc<Pipeline>,
        _vert_s: vs::Shader,
        _frag_s: fs::Shader,
    }

    impl CirclesGfx {
        pub fn new(
            device: Arc<Device>,
            render_pass: Arc<dyn RenderPassAbstract + Send + Sync>,
        ) -> Self {
            let vert_s = vs::Shader::load(device.clone()).unwrap();
            let frag_s = fs::Shader::load(device.clone()).unwrap();
            let vertices = Vec::new();

            let pipeline = Arc::new(
                GraphicsPipeline::start()
                    .vertex_input_single_buffer::<Vertex>()
                    .vertex_shader(vert_s.main_entry_point(), ())
                    .point_list()
                    .viewports_dynamic_scissors_irrelevant(1)
                    .fragment_shader(frag_s.main_entry_point(), ())
                    .render_pass(Subpass::from(render_pass, 0).unwrap())
                    .blend_alpha_blending()
                    .build(device.clone())
                    .unwrap(),
            );

            Self {
                vertices,
                pipeline,
                _vert_s: vert_s,
                _frag_s: frag_s,
            }
        }

        pub fn set_vertices(&mut self, vertices: &[Vertex]) {
            self.vertices = vertices.to_vec();
        }

        pub fn pipeline(&self) -> &Arc<Pipeline> {
            &self.pipeline
        }

        pub fn vertices(&self) -> &[Vertex] {
            &self.vertices
        }

        pub fn vertices_iter<'a>(
            &'a self,
        ) -> impl Iterator<Item = Vertex> + ExactSizeIterator + 'a {
            self.vertices.iter().copied()
        }
    }
}

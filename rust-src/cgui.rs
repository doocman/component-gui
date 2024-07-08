use sdl2::rect::Rect;
use sdl2::video::Window;

pub struct Sdl<'a> {
    window: &'a Window
}

impl<'a> Sdl<'a> {
    pub fn new(window: &'a sdl2::video::Window) -> Sdl<'a> {
        return Self{ window};
    }
    pub fn screen_area(&self) -> sdl2::rect::Rect {
        Rect::new(1, 1, 1, 1)
    }
}

pub struct ComponentBuilder {
}

pub struct Widget {}

impl ComponentBuilder {
    pub fn area<T>(&self, _: T) -> &Self { self }
    pub fn display<T>(&self, _: T) -> &Self { self }

    pub fn build(&self) -> Result<Widget, &'static str> {
        Err("Not yet implemented")
    }
}


impl Widget {
}

pub fn text_box_widget<T>(ctx: &T) -> ComponentBuilder {
    ComponentBuilder{}
}



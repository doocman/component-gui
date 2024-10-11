
#include <iostream>
#include <stdexcept>

#include <cgui/cgui.hpp>
#include <cgui/embedded/cgui_example_font.hpp>
#include <cgui/ft_fonts.hpp>
#include <cgui/sdl.hpp>

int main(int argc, char **argv) {
  try {
    auto sdl_context = build(cgui::sdl_context()).value();
    auto video_subsystem = video(sdl_context).value();
    auto main_window =
        build(window(video_subsystem, "Window Title - Hello World", 1024,
                     768)(cgui::sdl_window_resizable))
            .value();

    auto full_area = main_window.local_area();

    auto text_library = cgui::ft_font_library::init().value();
    auto dpi_info =
        main_window.dpi().value_or(cgui::sdl_display_dpi{72.f, 72.f, 72.f});
    auto text_font = cgui::ft_font_face::init(
                         text_library, cgui::embedded::cgui_example_font(),
                         static_cast<FT_UInt>(dpi_info.vert),
                         static_cast<FT_UInt>(dpi_info.hori))
                         .value();
    auto cached_font = cgui::cached_font(std::move(text_font));

    bool do_exit{};
    auto renderer = main_window.canvas().value();

    using area_t = decltype(full_area);

    auto gui =
        cgui::gui_context_builder()
            .widgets(cgui::widget_builder()
                         .area(area_t{})
                         .event(button_list_trigger())
                         .state(radio_button_state())
                         .subcomponents(vertical_widget_list<std::vector>(
                             cgui::widget_builder().display()) //
                             )
                         .build())
            .on_resize([]() {})
            .build();

  } catch (std::exception const &e) {
    std::cout << "Uncaught exception: " << e.what() << '\n';
  } catch (...) {
    std::cout << "Uncaught exception UNKNOWN\n";
  }
}

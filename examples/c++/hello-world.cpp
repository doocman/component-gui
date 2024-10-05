
#include <cstdlib>
#include <iostream>
#include <thread>

#include <SDL_main.h>

#include <cgui/cgui.hpp>
#include <cgui/embedded/cgui_example_font.hpp>
#include <cgui/ft_fonts.hpp>
#include <cgui/sdl.hpp>

int main(int, char **) {
  try {
    auto sdl_context = build(cgui::sdl_context()).value();
    auto video_subsystem = video(sdl_context).value();
    std::cout << " Set up window\n";
    auto main_window =
        build(window(video_subsystem, "Window Title - Hello World", 1024,
                     768)(cgui::sdl_window_resizable))
            .value();
    std::cout << " Window is here\n";

    auto full_area = main_window.local_area();

    auto text_library = cgui::ft_font_library::init().value();
    std::cout << " Gen ft_fonts\n";
    auto dpi_info =
        main_window.dpi().value_or(cgui::sdl_display_dpi{72.f, 72.f, 72.f});
    auto text_font = cgui::ft_font_face::init(
                         text_library, cgui::embedded::cgui_example_font(),
                         static_cast<FT_UInt>(dpi_info.vert),
                         static_cast<FT_UInt>(dpi_info.hori))
                         .value();
    std::cout << "font_face generated\n";
    auto cached_font = cgui::cached_font(std::move(text_font));
    auto hello_world_header =
        cgui::widget_builder()
            .area(cgui::call::trim_from_above(
                &full_area, std::min<int>(cgui::call::height(full_area), 128)))
            .display(cgui::text_renderer(std::ref(cached_font)))
            .build();
    std::get<0>(hello_world_header.displays())
        .set_displayed(hello_world_header.area(), "Hello World!")
        .text_colour({255, 255, 255, 255});
    auto button_bar_area = cgui::call::trim_from_below(
        &full_area, std::min<int>(cgui::call::height(full_area), 128));
    auto quit_button =
        cgui::widget_builder()
            .area(cgui::call::trim_from_left(
                &button_bar_area, cgui::call::width(button_bar_area) / 2))
            .display(cgui::display_per_state(cgui::fill_rect()),
                     cgui::text_renderer(std::ref(cached_font)))
            .event(cgui::buttonlike_trigger())
            .state(cgui::momentary_button{.on_click = [] {}})
            .build();
    std::get<1>(quit_button.displays())
        .set_displayed(quit_button.area(), "Quit")
        .text_colour({255, 255, 255, 255});
    {
      auto &background = std::get<0>(quit_button.displays());
      using enum cgui::momentary_button_states;
      get<off>(background).colour() = {40, 40, 40, 255};
      get<hover>(background).colour() = {190, 190, 190, 255};
      get<hold>(background).colour() = {63, 63, 63, 255};
    }

    auto lorum_ipsum = cgui::widget_builder()
                           .area(full_area)
                           .display(cgui::text_renderer(std::ref(cached_font)))
                           .build();
    std::get<0>(lorum_ipsum.displays())
        .set_displayed(
            hello_world_header.area(),
#if 1
            "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed "
            "do eiusmod tempor incididunt ut labore et dolore magna "
            "aliqua. Ut enim ad minim veniam, quis nostrud exercitation "
            "ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis "
            "aute irure dolor in reprehenderit in voluptate velit esse "
            "cillum dolore eu fugiat nulla pariatur. Excepteur sint "
            "occaecat cupidatat non proident, sunt in culpa qui officia "
            "deserunt mollit anim id est laborum.\n\nDid you get that?"
#else
            "h\nMuch text bad performance. Fix?"
#endif

            )
        .text_colour({255, 255, 255, 255});
    bool do_exit{};
    auto renderer = main_window.canvas().value();
    auto gui = cgui::gui_context(renderer).with(hello_world_header, lorum_ipsum,
                                                quit_button);

    auto gui = cgui::gui_context(renderer).with(/*widgets...*/).on_resize([] (cgui::bounding_box auto const& sz, auto&& widgets) {}).build();


    gui.render(renderer);
    renderer.present();
    using namespace std::chrono;
    constexpr auto run_interval = duration_cast<nanoseconds>(1s) / 60;

    auto next_run = steady_clock::now() + run_interval;
    while (!do_exit) {
      SDL_Rect to_rerender{};
      while (cgui::poll_event(sdl_context, [&]<typename T>(T e) {
               if constexpr (std::is_same_v<T, cgui::sdl_quit_event>) {
                 do_exit = true;
               } else if constexpr (cgui::has_handle<decltype(gui), T>) {
                 to_rerender = gui.handle(std::move(e));
               } else {
                 std::get<0>(hello_world_header.displays())
                     .set_displayed(hello_world_header.area(),
                                    typeid(T).name());
                 to_rerender = hello_world_header.area();
               }
               cgui::unused(e);
             }) != 0) {
      }
      if (!cgui::empty_box(to_rerender)) {
        gui.render(renderer, to_rerender);
        renderer.present();
      }
      std::this_thread::sleep_until(next_run);
      next_run += run_interval;
    }
    return EXIT_SUCCESS;
  } catch (std::exception const &e) {
    std::cout << "Uncaught exception: " << e.what() << '\n';
  }
  return EXIT_FAILURE;
}


#include <cstdlib>
#include <iostream>
#include <thread>

// #include <SDL_main.h>

#include <cgui/cgui.hpp>
#include <cgui/embedded/cgui_example_font.hpp>
#include <cgui/ft_fonts.hpp>
#include <cgui/sdl.hpp>

int main(int, char **) {
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
    auto renderer = main_window.renderer().value();

    using area_t = decltype(full_area);
    auto gui =
        cgui::gui_context_builder()
            .widgets(

                cgui::widget_builder().area(area_t{}).display(
                    cgui::text_renderer(std::ref(cached_font))),
                cgui::widget_builder()
                    .area(area_t{})
                    .display(cgui::display_per_state(cgui::fill_rect()),
                             cgui::text_renderer(std::ref(cached_font)))
                    .event(cgui::buttonlike_trigger(
                        cgui::momentary_button{}
                            .click([&do_exit] { do_exit = true; })
                            .build())),
                cgui::widget_builder()
                    .area(area_t{})
                    .display(cgui::fill_rect(),
                             cgui::display_per_state(
                                 cgui::text_renderer(std::ref(cached_font))))
                    .event(cgui::buttonlike_trigger(
                        cgui::toggle_button_state().build())),
                cgui::widget_builder().area(area_t{}).display(
                    cgui::text_renderer(std::ref(cached_font))) //
                )
            .on_resize([](cgui::size_wh auto const &sz, auto &&widgets) {
              using namespace cgui;
              auto &[hello_world_header, quit_button, random_toggle,
                     lorum_ipsum] = widgets;
              auto full_area = cgui::box_from_xywh<area_t>(
                  0, 0, call::width(sz), call::height(sz));
              hello_world_header.area(trim_from_above(
                  &full_area,
                  std::min<int>(cgui::call::height(full_area), 64)));
              std::get<0>(hello_world_header.displays())
                  .set_displayed(hello_world_header.area(), "Hello World!")
                  .text_colour({255, 255, 255, 255});
              auto button_bar_area = cgui::trim_from_below(
                  &full_area,
                  std::min<int>(cgui::call::height(full_area), 128));
              {
                auto &background = std::get<0>(quit_button.displays());
                using enum cgui::momentary_button_states;
                get<off>(background).colour() = {40, 40, 40, 255};
                get<hover>(background).colour() = {190, 190, 190, 255};
                get<hold>(background).colour() = {63, 63, 63, 255};
              }
              quit_button.area(cgui::trim_from_left(
                  &button_bar_area, cgui::call::width(button_bar_area) / 2));
              random_toggle.area(button_bar_area);
              std::get<0>(random_toggle.displays()).colour() = {127, 0, 0, 255};
              {
                auto &texts = std::get<1>(random_toggle.displays());
                using enum cgui::toggle_button_states;
                auto a = random_toggle.area();
                get<relaxed_off>(texts).set_displayed(
                    a, "I'm a happy off'ed button");
                get<hover_off>(texts).set_displayed(
                    a, "Don't you dare to click me!");
                get<hold_off>(texts).set_displayed(a, "You are clicking...");
                get<relaxed_on>(texts).set_displayed(
                    a, "Noo, come back and fix this. You must click me again!");
                get<hover_on>(texts).set_displayed(a,
                                                   "Click me back please...");
                get<hold_on>(texts).set_displayed(
                    a, "Pressing, and now... let it goooo!");
              }

              std::get<1>(quit_button.displays())
                  .set_displayed(quit_button.area(), "Quit")
                  .text_colour({255, 255, 255, 255});

              lorum_ipsum.area(full_area);
              std::get<0>(lorum_ipsum.displays())
                  .set_displayed(
                      lorum_ipsum.area(),
                      "Lorem ipsum dolor sit amet, consectetur adipiscing "
                      "elit, sed do eiusmod tempor incididunt ut labore et "
                      "dolore magna aliqua. Ut enim ad minim veniam, quis "
                      "nostrud exercitation ullamco laboris nisi ut aliquip ex "
                      "ea commodo consequat. Duis aute irure dolor in "
                      "reprehenderit in voluptate velit esse cillum dolore eu "
                      "fugiat nulla pariatur. Excepteur sint occaecat "
                      "cupidatat non proident, sunt in culpa qui officia "
                      "deserunt mollit anim id est laborum.\n\nDid you get "
                      "that?")
                  .text_colour({255, 255, 255, 255});
            })
            .build(full_area);

    renderer.clear();
    renderer.render_to(gui);
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
                 auto new_rerender = gui.handle(std::move(e));
                 to_rerender = cgui::box_add(to_rerender, new_rerender);
               }
               cgui::unused(e);
             }) != 0) {
      }
      if (!cgui::empty_box(to_rerender)) {
        // This is not done automatically by the gui context as it could be used
        // to overlay a gui on top of other graphics (like a menu in a game).
        renderer.clear();
        if (cgui::box_includes_box(to_rerender, renderer.area())) {
          // This weird little thing is currently needed. It may be an error in
          // SDL3. A closer inspection is warranted. Follow issue at:
          // https://github.com/libsdl-org/SDL/issues/11401
          renderer.present();
          renderer.clear();
        }
        renderer.render_to(gui, to_rerender);
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

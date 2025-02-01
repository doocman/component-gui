
#include <chrono>
#include <iostream>
#include <stdexcept>

// GCC-11 is currently the GCC version used on the Github actions, and it does
// not support <format>
#include <fmt/chrono.h>

#include <cgui/cgui.hpp>
#include <cgui/dynamic.hpp>
#include <cgui/embedded/cgui_example_font.hpp>
#include <cgui/ft_fonts.hpp>
#include <cgui/sdl.hpp>

constexpr auto gen_radio_buttons(auto &cached_font) {
  return cgui::widget_builder()
      .event(
          cgui::radio_button_trigger()
              .elements(cgui::dynamic::uni_sized_widget_list_builder().displays(
                  cgui::display_per_state(cgui::fill_rect()),
                  cgui::text_renderer(std::ref(cached_font))))
              .build())
      .build();
}

constexpr auto gen_viewed_widget(auto &&w) {
  static_assert(cgui::view_port_trigger::sub_widget<decltype(w)>);
  return cgui::widget_builder()
      .event(cgui::view_port_trigger::builder()
                 .view(std::forward<decltype(w)>(w))
                 .build())
      .build();
}

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
    auto renderer = main_window.renderer().value();

    // TODO: Remove this variables by adding sensible logic into the gui
    bool rerender_all{};
    // TODO: Fix render-beyond-borders bug for button list.

    auto gui =
        cgui::gui_context_builder()
            .on_resize([](auto &&sz, auto &&ws) {
              auto &[w, end_button] = ws;
              auto full_area = cgui::box_from_xywh<cgui::default_point_rect>(
                  0, 0, cgui::call::width(sz), cgui::call::height(sz));
              auto button_area = cgui::trim_from_below(
                  &full_area, std::min(cgui::point_unit(120),
                                       cgui::call::height(full_area)));
              end_button.area(button_area);
              cgui::call::area(w, full_area);
            })
            .widgets(gen_viewed_widget(gen_radio_buttons(cached_font)),
                     cgui::widget_builder()
                         .event(cgui::buttonlike_trigger(
                             cgui::momentary_button()
                                 .click([&do_exit] { do_exit = true; })
                                 .build()))
                         .display(cgui::text_renderer(std::ref(cached_font)))
                         .build())
            .build(full_area);
    auto &[list, end_button] = gui.widgets();
    list.event_component().mutate_viewed([&](auto &&viewed) {
      viewed.event_component().mutate_elements([argc, argv, &end_button,

                                                &rerender_all](auto &elements) {
        auto prototype = elements.display_prototype();
        auto &[background, textr] = prototype;
        using enum cgui::toggle_button_states;
        get<relaxed_off>(background).colour() = {100, 40, 40, 255};
        get<relaxed_on>(background).colour() = {40, 40, 100, 255};
        get<hover_off>(background).colour() = {140, 100, 100, 255};
        get<hover_on>(background).colour() = {100, 100, 140, 255};
        get<hold_off>(background).colour() = {80, 10, 10, 255};
        get<hold_on>(background).colour() = {10, 10, 80, 255};
        textr.text_colour({255, 255, 255, 255});
        decltype(auto) f_prototype = elements.function_prototype();
        auto display_creator = [&prototype, &f_prototype, &end_button,
                                &rerender_all, &dest = elements.list()](
                                   std::string_view text) {
          auto disp = prototype;
          std::get<1>(disp).set_text(text);
          auto f = f_prototype;
          f.get(cgui::radio_button::trigger_on{}) =
              [&end_button, &rerender_all,
               ftext = fmt::format("End program (last button clicked was {})",
                                   text)] {
                auto &[textrenderer] = end_button.displays();
                textrenderer.set_text(ftext);
                rerender_all = true;
              };
          dest.emplace_back(std::move(disp), std::move(f));
        };
        display_creator("This is a list");
        display_creator("containing the");
        display_creator("arguments from");
        display_creator("main:");
        for (int i = 0; i < argc; ++i) {
          display_creator(argv[i]);
        }
      });
    });
    {
      auto &[etxt] = end_button.displays();
      etxt.text_colour({255, 255, 255, 255});
      etxt.set_text("End program");
    }

    renderer.clear();
    renderer.render_to(gui);
    renderer.present();
    using namespace std::chrono;
    constexpr auto fps = 60;
    constexpr auto run_interval = duration_cast<nanoseconds>(1s) / fps;

    auto next_run = steady_clock::now() + run_interval;
    while (!do_exit) {
      cgui::point_unit_t<SDL_Rect> to_rerender{};
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
      // TODO: This first branch should be removed.
      if (rerender_all) {
        renderer.clear();
        renderer.render_to(gui);
        renderer.present();
      } else if (!cgui::empty_box(to_rerender)) {
        renderer.clear();
        renderer.render_to(gui, to_rerender);
        renderer.present();
      }
      std::this_thread::sleep_until(next_run);
      next_run += run_interval;
    }

    return EXIT_SUCCESS;

  } catch (std::exception const &e) {
    std::cout << "Uncaught exception: " << e.what() << '\n';
  } catch (...) {
    std::cout << "Uncaught exception UNKNOWN\n";
  }
  return EXIT_FAILURE;
}

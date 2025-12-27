
#ifndef ASPECT_GUI_MACRO_INC_ASP_COMPAT_HPP
#define ASPECT_GUI_MACRO_INC_ASP_COMPAT_HPP

#if __cpp_static_call_operator >= 202207L
#define ASP_STATIC_CALL static
#define ASP_STATIC_CALL_POST
#else
#define ASP_STATIC_CALL
#define ASP_STATIC_CALL_POST const
#endif

#endif

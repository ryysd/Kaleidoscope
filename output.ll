; ModuleID = 'Kaleidoscope'

define double @add(double %a, double %b) {
entry:
  %addtmp = fadd double %a, %b
  ret double %addtmp
}

define double @main() {
entry:
  %calltmp = call double @add(double 1.000000e+00, double 2.000000e+00)
  ret double %calltmp
}

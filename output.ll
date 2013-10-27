; ModuleID = 'Kaleidoscope'

define double @fib(double %x) {
entry:
  %cmptmp = fcmp ult double %x, 3.000000e+00
  br i1 %cmptmp, label %ifcont, label %else

else:                                             ; preds = %entry
  %subtmp = fadd double -1.000000e+00, %x
  %calltmp = call double @fib(double %subtmp)
  %subtmp1 = fadd double -2.000000e+00, %x
  %calltmp2 = call double @fib(double %subtmp1)
  %addtmp = fadd double %calltmp, %calltmp2
  br label %ifcont

ifcont:                                           ; preds = %entry, %else
  %iftmp = phi double [ %addtmp, %else ], [ 1.000000e+00, %entry ]
  ret double %iftmp
}

define double @0() {
entry:
  %calltmp = call double @fib(double 4.000000e+01)
  ret double %calltmp
}

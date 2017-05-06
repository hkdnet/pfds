datatype shape = CIRC of real | RECT of real * real;

fun area (CIRC r) = 3.14 * r * r
  | area (RECT (w, h)) = w * h;

val c = CIRC 2.0;
val r = RECT (3.0, 4.0);
area c;
area r;
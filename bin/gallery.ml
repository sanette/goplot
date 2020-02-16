open Init

let dumb () = ref None

let example = List.rev
  [ ref (Color { Plt.r=1.; Plt.g=0.; Plt.b=0. });
    ref (Func ({ formula="sin(x)"; var="x"},
	       { min="-Pi"; max="Pi"}, dumb ()));
    ref (Color { Plt.r=0.; Plt.g=0.; Plt.b=0. });
    ref (Axis ({ x="0"; y="0" }, dumb ())) ]

  
let pinched_torus = 
  [ ref (Surface ({ formula2="(1+cos(u)/2*sin(v/2))*cos(v)/2"; var1="u"; var2="v"},
		  { formula2="(1+cos(u)/2*sin(v/2))*sin(v)/2"; var1="u"; var2="v"},
		  { formula2="sin(u)/4*sin(v/2)"; var1="u"; var2="v"},
		  { min="-Pi"; max="Pi"}, { min="0"; max="2*Pi"}, dumb ())) ]
      

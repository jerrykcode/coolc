class C {
	a : Int;
	b : Bool;
	init(x : Int, y : Bool) : C {
           {
		a <- x;
		b <- y;
		self;
           }
	};
};

Class Main {
	main():C {
	  (new C).init(1,true)
	};
};

class D inherits C {
    
    n:Int;
    f() : Int {
        n
    };

    f1(c:C, d:D): D {
        self
    };
    
    f2(x:Int): D {
        {
            let x : Int <- 3 in {
                while x < 100 loop
                    x <- x + 1
                pool;
                self;
            };
        }
    };

    f3() : Bool {
        let x:Bool in {
            x
        }
    };

    f4(x:Int, c: C, d:D, o:Object) : Object {
        self
    };

};

class E inherits C {
    e : E;
    func(n1 : Int, n2 :Int, n3 : Int, n4:Int) : SELF_TYPE {
        let x : Int <- 0, y : Int <- x, c:C, d:D in {
            let oldx:Int <- x, z:Int <- 
                while if n1<n2 then n1 else n2 fi < x loop  {x <- x - 1; x; } pool
            in {
                x <- oldx;
                if if if z <= n3 then n3 else z fi = n4 then n4 - 1 else n4 fi < d.f() then {
                    e <- let n5:Int <- d.f(), e1:E in  {
                        while if n5=(new D).f() then n5 else { (new IO).out_string("ERR~~~"); n5; } fi = y loop
                            if self = e then e1 <- self else e1<-new E fi
                        pool;
                        e1;
                    };
                    self;
                } else {
                    get_self()
                }fi;
            }
        }
    };

    get_self() : SELF_TYPE {
        self
    };
};

class F inherits D {

};

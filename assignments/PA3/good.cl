class A {
ana(): Int {
(let x:Int <- 1 in 2)+3
};
};

Class BB__ inherits A {
bnb():Int{
    520
};
};

Class CC inherits B {

    x:Int <- 3;
    y:Bool <- false;

    f1() : Int {
        1314
    };
    
    ff(a:Int, b:Int) : CC {
        (let c : Int <- x+a in while x<=b loop if y then {
            case var of 
               a:Int=> xxx;
               b:Int=> (let val:A <- new ABC in 54*777+val*haha );
               c:CC=> { 
                    aaa@BBB.function(888, 666, 666*666/666);
                    if aaa <= b then 5*5 fi;
               };
            esac;

          }  else y fi  pool)
    };
};

Class TEST_PARSER_ {
    vala:Int <- 1;
    objectA : A <- new A;
    valb: Int <- objectA.ana();
    objectB : BB__ <- new BB__;
    valc :Int <- objectB.bnb();
    objectC : CC <- new CC;
    vald: Int <- objectC.f1();
    objectD : CC <- objectC.ff(vala, valb);

    f1(): Int {
        520*1314
    };

    vale : Int <- f1();


    f_test_if() : Int {
        if vala < valb then 
            1
        else if valb < valc then
            2
        else if valc < vald then
            3
        else if vald < vale then
            4
        else 5
        fi
        fi
        fi            
        fi
    };
};

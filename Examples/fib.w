	//William Norman
    //428002797
    //314-500

var n = 10;
var x = n - 2;
var f = 0;

if(n==1){
	print "Fibonacci number ";
	print n;
	print ": ";
	print f;
	print "\n";
}else{
	var f1 = 0;
	var f2 = 1;
	f = f1 + f2;
	while (x > 1) {
		f1 = f2;
		f2 = f;
		f = f1 + f2;
		x = x - 1;
	}
	print "Fibonacci number ";
	print n;
	print ": ";
	print f;
	print "\n";
}
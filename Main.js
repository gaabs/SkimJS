function max(a, b){
    if ( a < b ){
        return b;
    } else {
        return a;
    }
}
var arrG = [1,3,5-1,-1,0,4];
	
function kadane(arr){
	var n = arr.length;
	var resposta = 0;
	var soma = 0;
	for (var i=0;i<n;i++){
		soma = soma + arr[i];
		if ( soma < 0 ){
			soma = 0;
		}
		resposta = max(resposta, soma);

	}
	while(n==6){
		ffff = 999;
		n = 9;
	}
	return resposta;
}

var a = kadane(arrG);
a;
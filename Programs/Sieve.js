var sieve = function(n){
	var primes = [];
	for(var i = 0; i <= n; i++) primes = primes.concat([true]);
	primes[0] = false;
	primes[1] = false;
	
	ans = [];
	for(var i = 0; i <= n; i++) {
		if (primes[i]){
			ans = ans.concat([i]);
			for(var j = i+i; j <= n; j += i){
				primes[j] = false;
			}
		}
	}
}

sieve(1000);
//console.log(ans);
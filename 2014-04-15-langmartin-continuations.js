function begin (handler, expr, callback, res) {
    if (expr.length === 0) return callback(res);
    var fn = expr[0], rest = expr.slice(1),
		retry = function() {
			begin(handler, expr, callback, res);
		},
		cont = function(err, res) {
			if (err) handler(err, retry);
			else begin(handler, rest, callback, res);
		};
    try {
        res = fn(res, cont);
        if (res !== undefined) cont(null, res);
    } catch (err) { handler(err, retry) }
}

// Totally bogus example procedures!

var counters = [1];

function errorHandler(err, retry) {
	alert(err);
	retry();
}

function login() {
	console.log("logging in...");
	return true;
}

function getCounters(login, cont) {
	cont(null, counters);
}

function first(arr) {
	return arr[0];
}

function inc(n) {
	return n + 1;
}

function pushCounter(counter, cont) {
	counters.push(counter);
	cont(null, {pushed: counter});
}

begin(
    errorHandler,
    [login,
     getCounters,
     first,
     inc,
     pushCounter],
    function(result) {
        console.log(result);
    });

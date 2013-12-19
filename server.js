var http  = require( 'http' );
http.createServer( function ( req, res ) {
	res.end( 'Hello World\n' );
}).listen( 8080 );
console.log( 'a' );

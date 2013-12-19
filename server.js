var express = require( 'express' );
var fs = require( 'fs' );
var app = express();

// Load Contollers
var contorllers = {};
contorllers[ 'index' ] = require( "./contorllers/index" );


// Load Views
var views = {};
views.ejs = require( 'ejs' );

views.templates = {}
views.templates[ 'index' ] = fs.readFileSync( './views/templates/index.html', { "encoding" : "utf8" } )

// Load Context
var context = {};
context.contorllers = contorllers;
context.views = views

app.get( '/', function( req, res ) {
    contorllers['index'].run( context, req, res );
});

app.listen( 8080 );
console.log('Server Loaded');

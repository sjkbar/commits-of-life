var express = require( 'express' );
var fs = require( 'fs' );
var _ = require("underscore");

//Initialize Express;
var app = express();

// Setting MiddleWare
app.use( express.bodyParser( { uploadDir :  "./uploads" } ) );

// Load Contollers
var contorllers = {};
contorllers[ 'index' ] = require( "./contorllers/index" );
contorllers[ 'tableUpload' ] = require( "./contorllers/tableUpload" );

// Load Views
var views = {};
views.ejs = require( 'ejs' );

views.templates = {}
views.templates[ 'index' ] = fs.readFileSync( './views/templates/index.html', { "encoding" : "utf8" } )
views.templates[ 'tableUpload' ] = fs.readFileSync( './views/templates/tableUpload.html', { "encoding" : "utf8" } )

// Load Context
var context = {};
context.contorllers = contorllers;
context.views = views

// Load Misc
context.misc = {};
context.misc.fs = fs;
context.misc._ = _;

// Main Page;
app.get( '/', function( req, res ) {
    contorllers['index'].run( context, req, res );
});

// Table Upload
app.get( '/tableUpload', function( req, res ) {
    contorllers['tableUpload'].run( context, req, res );
});
app.post( '/tableUpload', function( req, res ) {
    contorllers['tableUpload'].run( context, req, res );
});


// :: TODO ::
// Table Revert
// Run Server

app.listen( 8080 );
console.log('Server Loaded');

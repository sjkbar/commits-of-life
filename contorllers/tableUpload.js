exports.run = function( context, req, res )
{
  console.log( req.body );
  if( req.method === "POST" )
  {
    if( Array.isArray( req.files.tables ) )
    {
      var _ = context.misc._;
      _.each( req.files.tables, function( table ) {
        saveFile( context, table.path, table.name, function( err ){
          gitCommmit( context, req.body, function( err ){
          });
        });
      });
    }
    else
    {
      saveFile( context, req.files.tables.path, req.files.tables.name, function( err ){
        gitCommmit( context, req.body, function( err ){
        });
      });
    }
  }

  template = context.views.templates[ 'tableUpload' ];
  html = context.views.ejs.render( template, {} );
  res.send( html );
};


var saveFile = function( context, path, name, callback )
{
  var readOptions = { encoding : "utf8" };
  context.misc.fs.readFile( path, readOptions, function( err, data ) {
      if( err ) throw err;

      var writePath = context.configuration.git_path + "/" + name;
      var writeOptions = { flag : "w+", encoding : "utf8" };

      context.misc.fs.writeFile( writePath, data, writeOptions, function ( err ) {
        if( err ) throw err;
        callback( false );
      })
  });
};

var gitCommmit = function( context, commitMessage, callback )
{
  var async = require( "async" );
  async.series({
    chdirToGitPath : function( callback ){
      try
      {
        process.chdir( context.configuration.git_path )
        callback( null, process.cwd() );
      }
      catch( err )
      {
        callback( err, null );
      }
    },
    gitAdd : function( callback ){
      var exec = require( "child_process" ).exec;
      var child;

      var command = "git add -A"
      child = exec( command, function( err, stdout, stderr ){
        if ( err ) throw err;

        callback( null, stdout );
      });
    },
    gitCommit : function( callback ) {
      var exec = require( "child_process" ).exec;
      var child;


      var util = require('util');
      var command = util.format("git commit -a -m '[Title] %s' -m '[Update Date] %s' -m '[Description] %s'", commitMessage.title, commitMessage.date, commitMessage.description );
      console.log( command );
      child = exec( command, function( err, stdout, stderr ){
        if ( err ) throw err;

        callback( null, stdout );
      });
    },
    chdirToExecPath : function( callback ) {
      try
      {
        var path = require( "path" );
        process.chdir( path.dirname( process.argv[1] ) );
        callback( null, process.cwd() );
      }
      catch( err )
      {
        callback( err, null );
      }
    }
  }, function( err, results ){
    if( err ) throw err;
    console.log( results );
  });
};

exports.run = function( context, req, res )
{
  if( req.method === "POST" )
  {
    if( Array.isArray( req.files.tables ) )
    {
      var _ = context.misc._;
      _.each( req.files.tables, function( table ) {
        console.log( "path : " + table.path + ", name : " + table.name );
      });
    }
    else
    {
      console.log( "path : " + req.files.tables.path + ", name : " + req.files.tables.name );
    }
  }

  template = context.views.templates[ 'tableUpload' ];
  html = context.views.ejs.render( template, {} );
  res.send( html );
};


var saveFile = function( path, name, callback )
{
};

var gitCommmit = function( 

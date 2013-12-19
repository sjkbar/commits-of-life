exports.run = function( context, req, res )
{
  template = context.views.templates[ 'tableUpload' ];
  html = context.views.ejs.render( template, {} );
  res.send( html );
};

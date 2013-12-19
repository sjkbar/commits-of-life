exports.run = function( context, req, res )
{
  template = context.views.templates[ 'index' ];
  html = context.views.ejs.render( template, {} );
  res.send( html );
};

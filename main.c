#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "uv/include/uv.h"

uv_pipe_t* p_stdin_pipe;
uv_pipe_t* p_stdout_pipe;

typedef struct {
    uv_write_t req;
    uv_buf_t buf;
} write_req_t;

void on_write( uv_write_t* req, int status )
{
    printf("A");
    write_req_t* p_write_req = (write_req_t*) req;
    if( p_write_req->buf.base )
    {
        printf( "free write buf\n" );
        free( p_write_req->buf.base );
    }
}

void on_allocate_buffer( uv_handle_t* handle, size_t suggested_size, uv_buf_t* buf )
{
    buf->base = malloc( sizeof( char ) * suggested_size );
    buf->len = suggested_size;
    return;
}

void on_stdin_pipe_read( uv_stream_t* stream, ssize_t nread, const uv_buf_t* buf )
{
    if( nread < 0 )
    {
        if( nread == UV_EOF )
        {
            printf( "EOF : on_stdin_pipe_read\n" );
        }
        else
        {
            printf( "FAIL : on_stdin_pipe_read\n" );
        }
    }
    else
    {
        /*
        write_req_t* write_req = malloc( sizeof( write_req_t ) );
        write_req->buf = uv_buf_init( malloc( sizeof( char ) * nread ), nread );
        memcpy( write_req->buf.base, buf->base, nread );
        */

        uv_write_t write_req;
        uv_buf_t buf = uv_buf_init( "A", 1 );
        uv_write( &write_req, (uv_stream_t*) p_stdout_pipe, buf, 1, on_write );
    }

    if( buf->base )
    {
        printf( "free read buf\n" );
        free( buf->base );
    }
}

int main()
{
    // p_stdin_pipe initialize
    uv_pipe_t* p_stdin_pipe = malloc( sizeof( uv_pipe_t ) );
    uv_pipe_init( uv_default_loop(), p_stdin_pipe, 0);
    uv_pipe_open( p_stdin_pipe, 0 );

    // p_stdout_pipe initialize
    uv_pipe_t* p_stdout_pipe = malloc( sizeof( uv_pipe_t ) );
    uv_pipe_init( uv_default_loop(), p_stdout_pipe, 0);
    uv_pipe_open( p_stdout_pipe, 1 );

    // read pipe start
    uv_read_start( (uv_stream_t*) p_stdin_pipe, on_allocate_buffer, on_stdin_pipe_read );

    uv_run( uv_default_loop(), UV_RUN_DEFAULT );
    return 0;
}

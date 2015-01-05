#include "csapp.h"

#define BUFSIZE 8196
int main(int argc, char* argv[]) {
    if (argc != 4) {
        fprintf(stderr, "usage: %s <ip> <port> <message>\n", argv[0]);
        exit(0);
    }

    char* server_ip = argv[1];
    int server_port = atoi(argv[2]);
    char* message_str = argv[3];

    int server_fd = Open_clientfd(server_ip, server_port);
    if(server_fd < 0) {
        unix_error("open server fd fail\n");
    }

    rio_t rio_server;
    Rio_readinitb(&rio_server, server_fd);

    char server_buf[BUFSIZE];
    int i = 0;
    for(i = 0; i< BUFSIZE; i ++) {
        server_buf[i] = 0;
    }

    sprintf(server_buf, "%s %s?msg=%s %s", "GET", "/index.html", message_str, "HTTP/1.0 \r\n\r\n");
    printf("SEND : %s\n", server_buf);
    //sprintf(server_buf, "%s %s %s", "GET", "/index.html", "HTTP/1.0 \r\n\r\n");
    Rio_writen(server_fd, server_buf, strlen(server_buf));

    int read_size = 0;
    while((read_size = Rio_readlineb(&rio_server, server_buf, BUFSIZE)) != 0) {
        printf("%s", server_buf);
    }

    printf("\ndone! read_size : %d\n", read_size);
    return 0;
}

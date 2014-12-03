/*
 * proxy.c - CS:APP Web proxy
 *
 * Student ID: 
 *         Name:
 * 
 * IMPORTANT: Give a high level description of your code here. You
 * must also provide a header comment at the beginning of each
 * function that describes what that function does.
 */ 

#include "csapp.h"
#include <sys/shm.h>


/* The name of the proxy's log file */
#define PROXY_LOG "proxy.log"
#define CHILD_PROCESS_NUM 5

/* Undefine this if you don't want debugging output */
#define DEBUG

/*
 * Functions to define
 */
void *process_request(void* vargp);
int open_clientfd_ts(char *hostname, int port, sem_t *mutexp);
ssize_t Rio_readn_w(int fd, void *ptr, size_t nbytes);
ssize_t Rio_readlineb_w(rio_t *rp, void *usrbuf, size_t maxlen);
void Rio_writen_w(int fd, void *usrbuf, size_t n);


int MAX_CLI_BUF_LENGTH = MAXBUF;
int MAX_SERVER_BUF_LENGTH = MAXBUF;
int ECHO_SERVER_PORT = 5555;
char* ECHO_SERVER_NAME = "localhost";


void upper_case(char *s)
{
  while (*s) {
    *s = toupper(*s);
    s++;
  }
}

/*
 * main - Main routine for the proxy program
 */
key_t shm_key = 12312;
sem_t sem;
int log_fd;
pid_t pids[CHILD_PROCESS_NUM];

struct fork_arg {
    int client_fd;
    struct sockaddr_in client_sock_addr;
};

void sigint_handler_parent(int sig) {
    printf("[PARENT] RECV kill SIGINT\n");
    int i = 0;
    for(i=0; i < CHILD_PROCESS_NUM; i++) {
        kill(pids[i], SIGINT);
    }
}

void sigint_handler_child(int sig) {
    printf("[CHILD] RECV kill SIGINT\n");
    exit(0);
}

int main(int argc, char **argv)
{
    /* Check arguments */
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <port number>\n", argv[0]);
        exit(0);
    }

    //create shared memorry
    int shm_id;

    shm_id = shmget(shm_key, sizeof(int), IPC_CREAT | 0666);
    if (shm_id == -1) {
        fprintf(stderr, "Fail : shmget fail");
        exit(0);
    }

    //initialize shared memorry
    int* shared_memory = (int*) shmat(shm_id, (void *)0, 0);
    if (shared_memory == (int*) -1)
    {
        perror("shmat failed : ");
        exit(-1);
    }

    *shared_memory = 0;

    // open port
    int port = atoi(argv[1]);
    int linsten_fd = Open_listenfd(port);

    //initialize sempahore
    Sem_init(&sem, 1, 1);

    //create proxy.log
    log_fd = open("./proxy.log", O_CREAT | O_WRONLY | O_TRUNC, 0644);
    if(log_fd < 0) {
        fprintf(stderr, "Fail : open proxy.log");
        exit(-1);
    }

    // accept client
    struct sockaddr_in client_sock_addr;
    int client_sock_addr_length = sizeof(client_sock_addr);

    int i =0;
    for (i = 0; i < CHILD_PROCESS_NUM; ++i) {
        int p = fork();
        if (p == 0) {
            //CHILD
            while(1) {
                Signal(SIGINT, sigint_handler_child);

                int conn_fd = Accept(linsten_fd, ((SA *) &client_sock_addr), &client_sock_addr_length);

                struct fork_arg args;
                args.client_fd = conn_fd;
                args.client_sock_addr = client_sock_addr;

                process_request((void*) &args);
                printf("client_fd closed !!!0\n");
                Close(conn_fd);
                printf("close conn_fd\n");
            }
        }
        else if (p < 0) {
            fprintf(stderr, "fail fork\n");
        } else {
            //PARENT
            pids[i] = p;
        }
    }

    //Only parent can access
    Signal(SIGINT, sigint_handler_parent);

    printf("wait for reaping\n");
    int child_status = 0;
    for (i = 0; i < CHILD_PROCESS_NUM; i++) {
        pid_t wpid = wait(&child_status);
        if (WIFEXITED(child_status))
            printf("Child %d terminated with exit status %d\n", wpid, WEXITSTATUS(child_status));
        else
            printf("Child %d terminated abnormally\n", wpid);
    }

    printf("wait for alll process done\n");
    printf("close listend_fd\n");
    printf("close file_fd\n");
    Close(linsten_fd);
    Close(log_fd);

    return 0;
}

void *process_request(void* vargp) {
    printf("in process_request\n");
    struct fork_arg* p_fork_arg = (struct fork_arg*) vargp;
    int client_fd = p_fork_arg->client_fd;
    struct sockaddr_in client_sock_addr = p_fork_arg->client_sock_addr;
    char* ip = inet_ntoa(client_sock_addr.sin_addr);

    // open server_fd
    int server_fd = open_clientfd_ts(ECHO_SERVER_NAME, ECHO_SERVER_PORT, 0);

    //initialzie client rio
    rio_t rio_client;
    char client_buf[MAX_CLI_BUF_LENGTH];
    Rio_readinitb(&rio_client, client_fd);

    //initialzie server rio
    rio_t rio_sever;
    char server_buf[MAX_CLI_BUF_LENGTH];
    Rio_readinitb(&rio_sever, server_fd);

    int isHttpRequest = 0;
    //read client message
    //write serber message 
    ssize_t read_size = 0;
    while((read_size = Rio_readlineb_w(&rio_client, client_buf, MAX_CLI_BUF_LENGTH)) != 0) {
        printf("read_size : %d\n", read_size);
        printf("client_buf : %s\n", client_buf);

        char protocol[12];
        char protocol_name[12];
        char temp[MAXBUF];

        memset(protocol, 0, sizeof(protocol));
        memset(temp, 0, sizeof(temp));

        sscanf(client_buf, "%s %s %s", temp, temp, protocol);
        sscanf(protocol, "%[^'/']=%s", protocol_name, temp);

        upper_case(protocol_name);
        printf("protocol_name : %s\n", protocol_name);
        printf("protocol : %s\n", protocol);
        if (strcmp("HTTP", protocol_name) == 0)
            isHttpRequest = 1;

        Rio_writen_w(server_fd, client_buf, strlen(client_buf));

        //Lock proxy.log
        P(&sem);

        //attach shared memmory;
        int shm_id = shmget(shm_key, sizeof(int), 0);
        if (shm_id == -1) {
            fprintf(stderr, "Fail : shmget fail");
            return 0;
        }

        int* shared_memory = (int*) shmat(shm_id, (void *)0, 0);
        if (shared_memory == (int*) -1)
        {
            perror("shmat failed : ");
            return 0;
        }

        // initialize log contents
        char count[100];
        int i =0;
        for(i = 0; i < 100; i++) {
            count[i] = 0;
        }
        sprintf(count, "%d", ++(*shared_memory));

        // write log
        // Source ip: request count: message
        printf("%s\n", ip);
        printf("%s\n", count);
        dprintf(log_fd, "%s:%s:%s", ip, count, client_buf);

        //Unlock proxy.log
        V(&sem);

        // write server's response to client
        read_size = 0;
        if(isHttpRequest) {
            printf("http request !!!!|\n");
            while( (read_size = Rio_readlineb_w(&rio_sever, server_buf, MAX_SERVER_BUF_LENGTH)) != 0) {
                printf("read_size : %d\n", read_size);
                printf("strlen : %d\n", strlen(server_buf));
                printf("server_buf : %s\n", server_buf);
                Rio_writen_w(client_fd, server_buf, strlen(server_buf));
            }

            if(read_size == 0) {
                break;
            }

            printf("read_size !!!0 : %d\n", read_size);
        } else {
            printf("echo request !!!!|\n");
            while( (read_size = Rio_readlineb_w(&rio_sever, server_buf, MAX_SERVER_BUF_LENGTH)) != 0) {

                printf("read_size : %d\n", read_size);
                printf("strlen : %d\n", strlen(server_buf));
                printf("server_buf : %s\n", server_buf);
                Rio_writen_w(client_fd, server_buf, strlen(server_buf));

                if (read_size != MAX_SERVER_BUF_LENGTH) {
                    break;
                }
            }
        }
    }

    printf("server_fd closed !!!0 : %d\n", read_size);
    Close(server_fd);
    printf("proxy done %d\n", server_fd);
    return 0;
}

int open_clientfd_ts(char *hostname, int port, sem_t *mutexp) {
    int client_fd = Open_clientfd(hostname, port);

    //write log with sem_t;
    return client_fd;
}

ssize_t Rio_readn_w(int fd, void *ptr, size_t nbytes) {
    return Rio_readn(fd, ptr, nbytes);
}

ssize_t Rio_readlineb_w(rio_t *rp, void *usrbuf, size_t maxlen) {
    return Rio_readlineb(rp, usrbuf, maxlen);
}

void Rio_writen_w(int fd, void *usrbuf, size_t n) {
    Rio_writen(fd, usrbuf, n);
}

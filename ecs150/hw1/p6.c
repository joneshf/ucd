#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

int printChildError()
{
    printf("Error forking child\n");
    return errno;
}

typedef struct tree {
    pid_t pid;
    int proc;
    struct tree * left;
    struct tree * right;
} Tree;

Tree * treealloc()
{
    return (Tree *) malloc(sizeof(Tree));
}

Tree * addLeaf(Tree * t, pid_t pid, int proc)
{
    if (NULL == t) {
        t = treealloc();
        t->pid = pid;
        t->proc = proc;
        t->left = NULL;
        t->right = NULL;
    } else if (proc < t->proc) {
        t->left = addLeaf(t->left, pid, proc);
    } else if (proc > t->proc) {
        t->right = addLeaf(t->right, pid, proc);
    }

    return t;
}

Tree * mkTree(int depth, pid_t parent)
{
    int i = 2, count = 1 << depth;
    Tree * t = addLeaf(NULL, parent, 1);

    for (; i < count; ++i) {
        if (getpid() == parent) {
            pid_t child = fork();
            if (0 == child) {
                continue;
            } else if (-1 == child) {
                _Exit(printChildError());
            } else {
                t = addLeaf(t, child, i);
            }
        }
    }

    return t;
}

void printTree(Tree * t)
{
    int proc = t->proc;
    pid_t pid = t->pid;
    printf("I am process %d, my process identifier is %d.\n", proc, pid);
    if (NULL != t->left) {
        printTree(t->left);
        kill(t->left->pid, SIGKILL);
    }
    if (NULL != t->right) {
        printTree(t->right);
        kill(t->right->pid, SIGKILL);
    }
}

int printUsage()
{
    printf("Usage: process_tree <number>\n\n");

    return EINVAL;
}

int readError()
{
    printf("Error reading input\n");

    return errno;
}

int main(int argc, char const *argv[])
{
    if (2 != argc) {
        return printUsage();
    }

    int depth;

    int scanned = sscanf(argv[1], "%d", &depth);

    if (EOF == scanned) {
        return readError();
    }

    if (depth > 0) {
        pid_t parent = getpid();
        Tree * procTree = mkTree(depth, parent);
        if (getpid() == parent) {
            printTree(procTree);
        }
    }

    return 0;
}

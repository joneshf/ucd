#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

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

Tree * mkTree(int depth)
{
    Tree * t = treealloc();
    int i = 0;
    for (; i < (1 << depth); ++i) {
        t = addLeaf(t, getpid(), i);
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
    }
    if (NULL != t->right) {
        printTree(t->right);
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
        Tree * procTree = mkTree(depth);
        printTree(procTree);
    }

    return 0;
}

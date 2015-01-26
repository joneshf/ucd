#include <errno.h>
#include <stdio.h>
#include <unistd.h>

typedef enum Boolean {false, true} Boolean;

// Create a tree ADT, akin to Haskell version of:
// data Tree = Leaf Int Int | Branch Int Int Tree Tree
typedef enum TreeTy {LeafTy, BranchTy} TreeTy;

typedef struct {
    TreeTy ty;
    union {
        struct Leaf * leaf;
        struct Branch * branch;
    } tree;
} Tree;

typedef struct Leaf {
    pid_t pid;
    int proc;
} Leaf;

typedef struct Branch {
    pid_t pid;
    int proc;
    Tree * left;
    Tree * right;
} Branch;

Tree mkLeaf(pid_t pid, int proc)
{
    struct Leaf l = {pid, proc};
    Tree t;
    t.tree.leaf = &l;
    t.ty = LeafTy;
    return t;
}

Tree mkBranch(pid_t pid, int proc, Tree * left, Tree * right)
{
    struct Branch b = {pid, proc, left, right};
    Tree t;
    t.tree.branch = &b;
    t.ty = BranchTy;
    return t;
}

Tree mkTree(int depth, int level, int proc)
{
    if (1 < depth - level) {
        Tree left = mkTree(depth, level + 1, 2 * proc);
        Tree right = mkTree(depth, level + 1, 2 * proc + 1);
        return mkBranch(getpid(), proc, &left, &right);
    } else {
        return mkLeaf(getpid(), proc);
    }
}

Boolean isLeaf(Tree t)
{
    return t.ty == LeafTy ? true : false;
}

Boolean isBranch(Tree t)
{
    return t.ty == BranchTy ? true : false;
}

void printPidProc(pid_t pid, int proc)
{
    printf("I am process %d, my process identifier is %d.\n", proc, (int)pid);
}

void printLeaf(Leaf l)
{
    printPidProc(l.pid, l.proc);
}

void printBranch(Branch b)
{
    printPidProc(b.pid, b.proc);
}

void printTree(Tree t)
{
    if (isLeaf(t) == true) {
        printf("pid is: %d, proc is: %d\n", (int) (t.tree.leaf->pid), t.tree.leaf->proc);
        printf("printing leaf\n");
        printPidProc(t.tree.leaf->pid, t.tree.leaf->proc);
    } else {
        printf("printing branch\n");
        printBranch(*t.tree.branch);
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
        Tree procTree = mkTree(1, 0, 1);
        printf("pid is: %d, proc is: %d\n", (int) (procTree.tree.leaf->pid), procTree.tree.leaf->proc);
        printPidProc((procTree.tree.leaf->pid), procTree.tree.leaf->proc);
        printTree(procTree);
    }

    return 0;
}

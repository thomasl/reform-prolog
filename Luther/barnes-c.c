/* ************************************************************************* *
 *  Implementation of the Barnes-Hut Algorithm in Computing Gravitational    *
 *                         Effects upon N Bodies.			     *
 *									     *
 * File         : barnes-c.c (C version)				     *
 * Author       : Alexander Jean-Claude Bottema				     *
 * Title        : Implementation of the Barnes-Hut Algorithm in		     *
 *                Computing Gravitional Effects upon N bodies		     *
 * Version      : 0.00							     *
 * First rev.   : May 3 1995						     *
 * Revidated    : May 3 1995						     *
 *									     *
 *************************************************************************** */

/* ========================================================================= *
 * Data representation:                                                      *
 *                                                                           *
 * The QUAD-tree is represented as follows:                                  *
 *                                                                           *
 * A node is either a tree or a body element. Body element are the "leaves"  *
 * in the sense that they do not contain trees.                              *
 *                                                                           *
 * A tree is represented as an array of nodes (recalling that a node is      *
 * the disjunctive union of trees and bodies). The dimension of the array    *
 * is 4 for QUAD-trees and 8 for OCT-trees. The empty tree is represented    *
 * through a null pointer.						     *
 *                                                                           *
 * A body is represented as a structure of 3 components; its mass and its    *
 * relative (x,y)-position w.r.t. the region defined by the parent tree.     *
 * A bodies velocity and other characteristics is stored elsewhere, since it *
 * is not considered to be a part of the algorithm.                          *
 *                                                                           *
 * ========================================================================= */

/* ------------------------------------------------------------------------- *
 *  Dependencies							     *
 * ------------------------------------------------------------------------- */

#include <stdlib.h>
#include <math.h>
#include <unistd.h>
#include <signal.h>

#include "timing.h"

/* ------------------------------------------------------------------------- *
 *  Constants								     *
 * ------------------------------------------------------------------------- */

#define	K			*1024

#define	TreeDimension		4
#define SpaceDimension		20000.0
#define Theta			0.3
#define TimeStep		1000
#define EPSILON			0.000001

#define TREE_HEAP_SIZE		1024 K
#define MAX_STARS		4096

#define EmptyNode		((Node *)NULL)

/* ------------------------------------------------------------------------- *
 *  Predicates								     *
 * ------------------------------------------------------------------------- */

#define IsEmptyNode(n)		((n) == EmptyNode)
#define IsTypeTree(n)		((n) != EmptyNode && (n) -> type == TypeTree)
#define IsTypeBody(n)		((n) != EmptyNode && (n) -> type == TypeBody)

/* ------------------------------------------------------------------------- *
 *  Functions								     *
 * ------------------------------------------------------------------------- */

#define PosToQuadrant(dx,dy) \
	(((dx) >= 0) ? (((dy) >= 0) ? 0 : 3) : (((dy) >= 0) ? 1 : 2))

#define QuadrantToPos(q,d,dx,dy)                   \
	(((q) == 0) ? ((dx) =  (d), (dy) =  (d)) : \
	 ((q) == 1) ? ((dx) = -(d), (dy) =  (d)) : \
	 ((q) == 2) ? ((dx) = -(d), (dy) = -(d)) : \
		      ((dx) =  (d), (dy) = -(d)))

/* ------------------------------------------------------------------------- *
 *  Macros for memory allocation					     *
 * ------------------------------------------------------------------------- */

typedef char *HeapPtr;
#define AllocMem(p,n)		((void *)(((p) += (n)) - (n)))

/* ------------------------------------------------------------------------- *
 *  Element types							     *
 * ------------------------------------------------------------------------- */

typedef enum { TypeTree, TypeBody } NodeType;

typedef struct Node Node;

typedef struct
{
	double		mass;
	double		x, y;
} Body;

typedef struct 
{
	double		mass;
	Node		*children[TreeDimension];
} Tree;

struct Node
{
	NodeType	type;
	union
	{
		Tree	*tree;
		Body	*body;
	} du;                      /* du = disjunctive union */
};

typedef struct
{
	double		mass;
	double		x, y;
	double		vx, vy;
} Star;

/* ------------------------------------------------------------------------- *
 *  Forwards								     *
 * ------------------------------------------------------------------------- */

void print_tree(Node *);

/* ------------------------------------------------------------------------- *
 *  Constructors							     *
 * ------------------------------------------------------------------------- */

inline Body * constr_body(HeapPtr **p, double mass, double x, double y)
{
    Body *body = AllocMem(*p, sizeof(Body));
    body -> mass = mass;	/* I don't use GNU-C structure aggregates, */
    body -> x = x;		/* since it is not ANSI-C.		   */
    body -> y = y;

    return body;
}

inline Tree * constr_quad_tree(HeapPtr **p,
			       double mass,
			       Node *a, Node *b, Node *c, Node *d)
{
    Tree *tree = AllocMem(*p, sizeof(Tree));
    tree -> children[0] = a;
    tree -> children[1] = b;
    tree -> children[2] = c;
    tree -> children[3] = d;
    tree -> mass = mass;

    return tree;
}

inline Node * constr_node(HeapPtr **p, NodeType type, void *obj)
{
    Node *node = AllocMem(*p, sizeof(Node));
    node -> type = type;
    if (type == TypeTree)
	node -> du.tree = (Tree *)obj;
    else
	node -> du.body = (Body *)obj;

    return node;
}

/* ------------------------------------------------------------------------- *
 *  Selectors & Designators						     *
 * ------------------------------------------------------------------------- */

#define	GetTreeFromNode(n)	((n) -> du.tree)
#define GetBodyFromNode(n)	((n) -> du.body)

#define GetSubTree(tree,n)	((tree) -> children[(n)])
#define GetMassTree(tree)	((tree) -> mass)

#define SetSubTree(tree,n,node) ((tree) -> children[(n)] = (node))
#define SetMassTree(tree,m)	((tree) -> mass = (m))

#define GetXFromBody(body)	((body) -> x)
#define GetYFromBody(body)	((body) -> y)
#define GetMassFromBody(body)	((body) -> mass)

/* ------------------------------------------------------------------------- *
 *  insert_quad_tree							     *
 * ------------------------------------------------------------------------- */

Node * insert_quad_tree(HeapPtr **p, Node *root,
		        double m, double x, double y)
{
    Node	*parent, *n, *selected;
    double	origin_x = 0.0, origin_y = 0.0;
    double	d = SpaceDimension;
    int		quadrant = -1;

    for (parent = EmptyNode, n = root; IsTypeTree(n); parent = n, n = selected)
    {
	Tree		*t = GetTreeFromNode(n);
	double		odx, ody;

	SetMassTree(t, GetMassTree(t) + m);
	quadrant = PosToQuadrant(x - origin_x, y - origin_y);
	d /= 2;
	QuadrantToPos(quadrant, d, odx, ody);
	origin_x += odx, origin_y += ody;
	selected = GetSubTree(t, quadrant);
    }

    if (IsEmptyNode(n))	/* Have we reached an empty leaf? */
        return IsEmptyNode(parent) ?
	          constr_node(p, TypeBody, constr_body(p, m, x, y))
		: (SetSubTree(GetTreeFromNode(parent),quadrant,
			      constr_node(p, TypeBody,
			                  constr_body(p, m, x, y))), root);
    else /* ... we have a body-body conflict*/
    {
	Body		*body = GetBodyFromNode(n);
	double		x0 = GetXFromBody(body),
			y0 = GetYFromBody(body),
			m0 = GetMassFromBody(body);
	int		quad0, quad1;
	Tree		*new_parent = constr_quad_tree(p,m0+m,EmptyNode,
							      EmptyNode,
							      EmptyNode,
							      EmptyNode);

	if (IsEmptyNode(parent))
	    root = constr_node(p,TypeTree, new_parent);
	else
	    SetSubTree(GetTreeFromNode(parent),quadrant,
		       constr_node(p,TypeTree,new_parent));

	quad0 = PosToQuadrant(x0 - origin_x, y0 - origin_y);
	quad1 = PosToQuadrant(x - origin_x, y - origin_y);

	while (quad0 == quad1) /* ... until body-body conflict is resolved */
	{
	    double	odx, ody;
	    Tree	*new_tree = constr_quad_tree(p,m0+m,EmptyNode,
							    EmptyNode,
							    EmptyNode,
							    EmptyNode);
	    d /= 2;
	    QuadrantToPos(quad0, d, odx, ody);
	    origin_x += odx, origin_y += ody;
	    SetSubTree(new_parent,quad0,constr_node(p,TypeTree,new_tree));
	    new_parent = new_tree;

	    quad0 = PosToQuadrant(x0 - origin_x, y0 - origin_y);
	    quad1 = PosToQuadrant(x - origin_x, y - origin_y);
	}

	SetSubTree(new_parent,quad0,constr_node(p,TypeBody,
						constr_body(p,m0,x0,y0)));
	SetSubTree(new_parent,quad1,constr_node(p,TypeBody,
						constr_body(p,m,x,y)));

	return root;
    }
}

/* ------------------------------------------------------------------------- *
 *  Compute acceleration						     *
 * ------------------------------------------------------------------------- */

void compute_acceleration(Node *node,double d,double theta2,
			  double origin_x, double origin_y,
			  double x, double y,
			  double *acc_x, double *acc_y)
{
    if (IsEmptyNode(node))
	*acc_x = *acc_y = 0.0;
    else
    if (IsTypeBody(node))
    {
	Body	*body = GetBodyFromNode(node);
	double	dx = GetXFromBody(body) - x;
	double	dy = GetYFromBody(body) - y;
	double	r2 = dx*dx + dy*dy;
	double  divisor = r2 * sqrt(r2);

	if (divisor < EPSILON)
	    *acc_x = *acc_y = 0.0;
	else
	{
	    double expr = GetMassFromBody(body) / divisor;
	    *acc_x = dx * expr, *acc_y = dy * expr;
	}
    }
    else /* Node type is tree! */
    {
	double	dx = origin_x - x, dy = origin_y - y;
	double	r2 = dx*dx + dy*dy;

	if (d*d < theta2 * r2)	/* Ok to approximate */
	{
	    double	divisor = r2 * sqrt(r2);

	    if (divisor < EPSILON)
	        *acc_x = *acc_y = 0.0;
	    else
	    {
		Tree	*t   = GetTreeFromNode(node);
		double	expr = GetMassTree(t) / divisor;

	        *acc_x = dx * expr, *acc_y = dy * expr;
	    }
	}
	else	/* Compute partial accelerations obtained by each quad. */
	{
	    Tree	*t   = GetTreeFromNode(node);
	    double	d_halved = d / 2;
	    double	odx0, ody0, odx1, ody1, odx2, ody2, odx3, ody3;
	    double	ax0, ay0, ax1, ay1, ax2, ay2, ax3, ay3;

	    QuadrantToPos(0,d_halved,odx0,ody0);
	    compute_acceleration(GetSubTree(t,0),d_halved,theta2,
				 origin_x + odx0, origin_y + ody0,
				 x, y, &ax0, &ay0);
	    QuadrantToPos(1,d_halved,odx1,ody1);
	    compute_acceleration(GetSubTree(t,1),d_halved,theta2,
				 origin_x + odx1, origin_y + ody1,
				 x, y, &ax1, &ay1);
	    QuadrantToPos(2,d_halved,odx2,ody2);
	    compute_acceleration(GetSubTree(t,2),d_halved,theta2,
				 origin_x + odx2, origin_y + ody2,
				 x, y, &ax2, &ay2);
	    QuadrantToPos(3,d_halved,odx3,ody3);
	    compute_acceleration(GetSubTree(t,3),d_halved,theta2,
				 origin_x + odx3, origin_y + ody3,
				 x, y, &ax3, &ay3);
	    *acc_x = ax0 + ax1 + ax2 + ax3;
	    *acc_y = ay0 + ay1 + ay2 + ay3;
	}
    }
}

/* ------------------------------------------------------------------------- *
 *  print_tree								     *
 * ------------------------------------------------------------------------- */

void print_tree( Node *n )
{
    if (IsEmptyNode(n))
	printf("empty");
    else
    if (IsTypeBody(n))
    {
	Body *body = GetBodyFromNode(n);
	printf("<body %f %f %f>", GetMassFromBody(body),
				  GetXFromBody(body),
				  GetYFromBody(body));
    }
    else /* Type is tree */
    {
	Tree *tree = GetTreeFromNode(n);
	printf("tree %f [", GetMassTree(tree));
	print_tree(GetSubTree(tree,0));
	printf(",");
	print_tree(GetSubTree(tree,1));
	printf(",");
	print_tree(GetSubTree(tree,2));
	printf(",");
	print_tree(GetSubTree(tree,3));
	printf("]");
    }
}

/* ------------------------------------------------------------------------- *
 *  Compute tree							     *
 * ------------------------------------------------------------------------- */

Node * compute_tree( Star stars[], int n )
{
    Node	*root = EmptyNode;
    static char	tree_heap[TREE_HEAP_SIZE];
    HeapPtr	*p = (HeapPtr *)tree_heap;
    int		i;

    for (i = 0; i < n; i++)
	root = insert_quad_tree(&p,root,stars[i].mass,stars[i].x,stars[i].y);

    return root;
}

/* ------------------------------------------------------------------------- *
 *  compute_time_step							     *
 * ------------------------------------------------------------------------- */

void compute_time_step(Node *tree, Star stars[], int n)
{
    int		i;
    double	theta2 = Theta*Theta;

    for (i = 0; i < n; i++)
    {
	double acc_x, acc_y;

        compute_acceleration(tree,SpaceDimension,theta2,0.0,0.0,
			     stars[i].x, stars[i].y, &acc_x, &acc_y);
	stars[i].vx = stars[i].vx + TimeStep * acc_x;
	stars[i].vy = stars[i].vy + TimeStep * acc_y;
	stars[i].x = stars[i].x + TimeStep * stars[i].vx;
	stars[i].y = stars[i].y + TimeStep * stars[i].vy;
    }
}

/* ------------------------------------------------------------------------- *
 *  double fetch_float(char *str, char **new_str_ptr)			     *
 *      Fetch next float number from string 'str.' 'new_str_ptr' points to   *
 *	the location where to continue the search.                           *
 * ------------------------------------------------------------------------- */

double fetch_float(char *str, char **new_str_ptr)
{
    static char		number[1024];
    int			i;
    char		*n;

    while (!isdigit(*str) && *str != '-')
	str++;

    for (n = number; isdigit(*str) ||
		     *str == '.' || tolower(*str) == 'e' ||
		     *str == '-' || *str == '+'; str++, n++)
	{ *n = *str; }

    *n = '\0';

    *new_str_ptr = str;

    return atof(number);
}

/* ------------------------------------------------------------------------- *
 *  int read_stars(char *filename)					     *
 *      Read stars from the given file named 'filename.' Return the	     *
 *      number of stars read (0 indicates failure).			     *
 *									     *
 *  Each star is represented as a Prolog-term:				     *
 *	'star(M,X,Y,VX,VY).'						     *
 *									     *
 *  Where M,X,Y,VX,VY are float/double numbers.				     *
 * ------------------------------------------------------------------------- */

int read_stars(char *filename, Star stars[])
{
    static char		line[1024];
    char		*line_p;
    int			n;

    FILE *f = fopen(filename, "r");
    if (f == NULL)
	return 0;

    for (n = 0; !feof(f); n++)
    {
	if (fgets(line, 1024, f) == NULL)
	    break;
	stars[n].mass = fetch_float(line, &line_p);
	stars[n].x = fetch_float(line_p, &line_p);
	stars[n].y = fetch_float(line_p, &line_p);
	stars[n].vx = fetch_float(line_p, &line_p);
	stars[n].vy = fetch_float(line_p, &line_p);
    }

    fclose(f);

    return n;
}

/* ------------------------------------------------------------------------- *
 *  plot_stars								     *
 * ------------------------------------------------------------------------- */

void plot_stars(FILE *f, Star stars[], int n)
{
    int		i;

    for (i = 0; i < n; i++)
    {
	fprintf(f, "plot %lf %lf\n", stars[i].x, stars[i].y);
        fflush(f);
    }
}

/* ------------------------------------------------------------------------- *
 *                                  M A I N                                  *
 * ------------------------------------------------------------------------- */

int main()
{
    static char   scratch[512];
    static Star   stars[MAX_STARS];
    int		  n, i, fd, pid, catch;
    FILE	  *f;
    int		  mypipe[2];
    Node	  *quad_tree;

    int		  c = 1000;

/*
    static char	  test_heap[16384];
    HeapPtr	  *test_ptr = (HeapPtr *)test_heap;
    Node	  *test_node;

    test_node = constr_node(&test_ptr, TypeBody, constr_body(&test_ptr, 10.0, 42.0, 4711.0));
    print_tree(test_node);
    exit(0);
*/

    initialize_timer();

    n = read_stars("stars.db", stars);

    for (i = 0; i < n; i++)
	printf("star %2d %10.2lf %10.2lf %10.2lf %10.2lf %10.2lf\n",
		i, stars[i].mass, stars[i].x, stars[i].y,
		stars[i].vx, stars[i].vy);

    if (pipe(mypipe) == -1)
    {
	printf("Error: Couldn't create pipe.\n");
	exit(0);
    }

    if ((pid = fork()) == 0)
    {				/* This the child process */
	close(0);		/* Close stdin */
	dup(mypipe[0]);		/* Redirect stdin to mypipe[1] */
				/* Data written to mypipe[1] is
				   read from mypipe[0] */
	close(mypipe[1]);

        execlp("plot", "plot", NULL); /* Start external plot program */
			              /* Communication through pipe  */
	exit(0);
    }

    /* Parent process continues here */

    f = fdopen(mypipe[1], "w");
    fprintf(f, "clear\n");
    fflush(f);

    plot_stars(f, stars, n);

    do
    {
        reset_timer();
	quad_tree = compute_tree( stars, n );
	/* print_tree(quad_tree); printf("\n"); */
        compute_time_step( quad_tree, stars, n );
	catch = get_timer();
        plot_stars(f, stars, n);
	printf("runtime %d\n", catch);
        gets(scratch);
    } while (strcmp(scratch,"quit") != 0);

    fclose(f);

    kill(pid,SIGKILL);	/* Kill child process */
}

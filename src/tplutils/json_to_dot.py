import json


class Id(object):
    def __init__(self):
        super().__init__()
        self._i = 0

    @property
    def i(self):
        self._i += 1
        return self._i


def convert(infile, outfile):
    print(
        """
digraph G {
    graph [rankdir=LR]
    node [shape=box,
    style=solid
    ]
    edge [color="#00000088",
    dir=back
    ]
    """,
        file=outfile,
    )
    i = Id()
    for graph in json.load(infile):
        print_single_graph(graph, i, outfile)
    print("}", file=outfile)


def print_single_graph(graph, i, outfile):
    name_to_node = {}
    print("subgraph cluster{}{{".format(i.i), file=outfile)

    parents = []
    for target, deps in graph.items():
        target_str = _escape(target)
        parents.append(target_str)

    for target, deps in graph.items():
        target_str = _escape(target)
        _register_node(target_str, i, name_to_node, outfile)
        target_node = name_to_node[target_str]
        for dep_str in (_escape(dep) for dep in deps):
            if dep_str in parents:
                _register_node(dep_str, i, name_to_node, outfile)
            else:
                _register_filled_node(dep_str, i, name_to_node, outfile)
            print("{} -> {}".format(target_node, name_to_node[dep_str]), file=outfile)
    print("}", file=outfile)


def _register_node(name, i, name_to_node, outfile):
    if not (name in name_to_node):
        node = "n{}".format(i.i)
        name_to_node[name] = node
        print("{}[label={}]".format(node, name), file=outfile)


def _register_filled_node(name, i, name_to_node, outfile):
    if not (name in name_to_node):
        node = "n{}".format(i.i)
        name_to_node[name] = node
        print('{}[label={}, style = "solid,filled"]'.format(node, name), file=outfile)


def _escape(s):
    return '"{}"'.format(s.replace('"', r"\""))

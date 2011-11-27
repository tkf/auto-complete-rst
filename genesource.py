#!/usr/bin/env python

from docutils import nodes
import re
import operator


def getcontent(rstfile, ftype):
    if ftype == 'gz' or (ftype is None and rstfile.endswith('.gz')):
        import gzip
        return gzip.open(rstfile).read()
    else:
        return file(rstfile).read()


def getdoctree(rstfile, ftype):
    from docutils.core import publish_doctree
    return publish_doctree(getcontent(rstfile, ftype))


def concat(list_of_list):
    """
    Concatenate list of list into a list

    >>> concat([[1, 2, 3], [4], [5, 6]])
    [1, 2, 3, 4, 5, 6]

    """
    return reduce(operator.add, list_of_list, [])


def cond_field_name_is_directive_type(nd):
    return (isinstance(nd, nodes.field_name) and
            'Directive Type' in nd.astext())


def dirtypes_from_text(text):
    """
    Parse text and get directive types

    >>> dirtypes_from_text('"attention", "caution", "danger"')
    ['attention', 'caution', 'danger']
    >>> dirtypes_from_text('"header" and "footer"')
    ['header', 'footer']
    >>> dirtypes_from_text('"sectnum" or "section-autonumbering" (synonyms)')
    ['sectnum', 'section-autonumbering']

    """
    return RE_IN_DOUBLE_QUOTES.findall(text)

RE_IN_DOUBLE_QUOTES = re.compile(r'"(.+?)"')


def getdirtypes(nd):
    """
    Get a list of directive types from filed_name node (=`:Directive Type:`)

    The following node ``nd`` returns ['list-table']::

      :Directive Type: "list-table"
       ---- =nd -----

    """
    return dirtypes_from_text(
        nd.parent.traverse(nodes.field_body)[0].astext())


def directives(rstfile, ftype=None):
    field_lists = [
        x.parent.parent for x in
        getdoctree(rstfile, ftype).traverse(cond_field_name_is_directive_type)]

    return concat(map(getdirtypes, field_lists))


def cond_literal_in_title_has_role(nd):
    return (isinstance(nd, nodes.literal) and
            isinstance(nd.parent, nodes.title) and
            RE_ROLE.match(nd.astext()))

RE_ROLE = re.compile(r":.+?:")


def cond_field_name_is_aliases(nd):
    return (isinstance(nd, nodes.field_name) and
            'Alias' in nd.astext())


def role_aliases_from_text(text):
    """
    Parse text and get role aliases as a list of string

    >>> role_aliases_from_text('``:title:``, ``:t:``')
    ['title', 't']
    >>> role_aliases_from_text('None')
    []

    """
    return RE_IN_COLONS.findall(text)

RE_IN_COLONS = re.compile(r':(.+?):')


def roles(rstfile, ftype=None):
    doctree = getdoctree(rstfile, ftype)
    roles_in_title = [
        x.astext().strip(":") for x in
        doctree.traverse(cond_literal_in_title_has_role)]
    role_aliases = concat([
        role_aliases_from_text(
            x.parent.traverse(nodes.field_body)[0].astext())
        for x in doctree.traverse(cond_field_name_is_aliases)])
    return roles_in_title + role_aliases


def genelisp(docs_dir, directives_file, roles_file):
    import os
    import jinja2

    env = jinja2.Environment()
    template = env.from_string(TEMP_SOURCE)
    print template.render(
        directives=directives(os.path.join(docs_dir, directives_file)),
        roles=roles(os.path.join(docs_dir, roles_file)),
        )


TEMP_SOURCE = r"""
(defun auto-complete-rst-directives-candidates ()
  '({% for item in directives %}"{{ item }}::" {% endfor %}))

(defun auto-complete-rst-roles-candidates ()
  '({% for item in roles %}"{{ item }}:" {% endfor %}))
"""


def main():
    from argparse import ArgumentParser

    parser = ArgumentParser(
        description="Generate source from rst file")
    parser.add_argument(
        '--docs-dir', default='/usr/share/doc/python-docutils/docs/ref/rst/')
    parser.add_argument(
        '--directives-file', default='directives.txt.gz')
    parser.add_argument(
        '--roles-file', default='roles.txt.gz')
    args = parser.parse_args()
    genelisp(**vars(args))


if __name__ == '__main__':
    main()

#!/usr/bin/env python

from docutils.parsers.rst import directives, roles
from docutils.parsers.rst.languages import en

try:
    # these imports register directives/roles defined in sphinx
    import sphinx.directives
    import sphinx.roles
except:
    pass


def get_directives_sub_modules():
    sub_module_names = list(set(
        m for (d, (m, c)) in directives._directive_registry.iteritems()))
    sub_modules = __import__(
        directives.__name__, globals(), locals(), sub_module_names, -1)
    return sub_modules


def get_directive_specs():
    sub_modules = get_directives_sub_modules()
    directive_specs = []

    def directive_specs_add(dirname, clsobj):
        directive_specs.append({
            'directive': dirname,
            'option': list(clsobj.option_spec if clsobj.option_spec else []),
            })

    for (dirname, (modname, clsname),
         ) in directives._directive_registry.iteritems():
        clsobj = getattr(getattr(sub_modules, modname), clsname)
        directive_specs_add(dirname, clsobj)

    for (dirname, clsobj) in directives._directives.iteritems():
        directive_specs_add(dirname, clsobj)

    return directive_specs


def get_roles():
    return sorted(
        set(roles._roles) | set(roles._role_registry) | set(en.roles))


def genelisp():
    import jinja2

    directive_specs = get_directive_specs()
    env = jinja2.Environment()
    template = env.from_string(TEMP_SOURCE)
    print template.render(
        roles=get_roles(),
        directive_specs=directive_specs,
        )


TEMP_SOURCE = r"""
(defun auto-complete-rst-directives-candidates ()
  '({% for spec in directive_specs %}"{{ spec.directive }}::" {% endfor %}))

(defun auto-complete-rst-roles-candidates ()
  '({% for item in roles %}"{{ item }}:" {% endfor %}))

{% for spec in directive_specs -%}
(puthash "{{ spec.directive }}"
         '({% for item in spec.option %}"{{ item }}:" {% endfor %})
         auto-complete-rst-directive-options-map)
{% endfor %}
"""


def main():
    from argparse import ArgumentParser

    parser = ArgumentParser(
        description="Generate source from rst file")
    args = parser.parse_args()
    genelisp(**vars(args))


if __name__ == '__main__':
    main()

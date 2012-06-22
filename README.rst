=====================================================
 Auto-completion for reStructuredText and Sphinx
=====================================================

auto-complete-rst is a source for
`Auto Complete Mode <http://cx4a.org/software/auto-complete/>`_
to help editing reStructuredText_ including Sphinx_ document.

.. _reStructuredText: http://docutils.sourceforge.net/rst.html
.. _Sphinx: http://sphinx.pocoo.org/


Minimum setup::

  (require 'auto-complete-rst)
  (auto-complete-rst-init)


Configurations
==============

``auto-complete-rst-other-sources``
-----------------------------------

This configuration variable specifies other sources to use in rst-mode.
Default ``ac-sources`` will be used if it is ``nil`` (default).

Example::

  (setq auto-complete-rst-other-sources
        '(ac-source-filename
          ac-source-abbrev
          ac-source-dictionary
          ac-source-yasnippet))


``auto-complete-rst-sphinx-extensions``
---------------------------------------

This configuration variable specifies Sphinx extension to add.

Example::

  (setq auto-complete-rst-sphinx-extensions
        '("PATH/TO/SOME/EXTENSION.py"
          "sphinx.ext.todo"))

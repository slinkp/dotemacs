#!/usr/bin/env python

"""
Combines pep8 and pyflakes output for emacs;
treats pep8 as warnings, pyflakes as errors.

From https://bitbucket.org/keegan_csmith/dotfiles/raw/tip/misc/pyflakespep8.py
modified a bit by slinkp.
"""


import commands
import re
import sys


def make_re(*msgs):
    return re.compile('(%s)' % '|'.join(msgs))

pyflakes_ignore = make_re('unable to detect undefined names')
pyflakes_ignore = []
pyflakes_warning = make_re(
    'imported but unused',
    'is assigned to but never used',
    'redefinition of unused',
    'unable to detect undefined names',
)

pep8_ignore = [
    # E101 indentation contains mixed spaces and tabs
    # E111 indentation is not a multiple of four
    # E112 expected an indented block
    # E113 unexpected indentation
    # E201 whitespace after '%s'
    # E202 whitespace before '%s'
    # E203 whitespace before '%s'
    # E211 whitespace before '%s'
    # E221 multiple spaces before operator
    # E222 multiple spaces after operator
    # E223 tab before operator
    # E224 tab after operator
    # E225 missing whitespace around operator
    # E225 missing whitespace around operator
    # E231 missing whitespace after '%s'
    # E241 multiple spaces after '%s'
    # E242 tab after '%s'
    # E251 no spaces around keyword / parameter equals
    # E261 at least two spaces before inline comment
    # E262 inline comment should start with '# '
    # E301 expected 1 blank line, found 0
    # E302 expected 2 blank lines, found %d
    # E303 too many blank lines (%d)
    # E304 blank lines found after function decorator
    # E401 multiple imports on one line
    # E501 line too long (%d characters)  -- see also --max-line-length
    # E701 multiple statements on one line (colon)
    # E702 multiple statements on one line (semicolon)
    # W191 indentation contains tabs
    # W291 trailing whitespace
    # W292 no newline at end of file
    'W293',  # blank line contains whitespace  # Bitly
    'W391',  # blank line at end of file  # Bitly
    # W601 .has_key() is deprecated, use 'in'
    # W602 deprecated form of raising exception
    # W603 '<>' is deprecated, use '!='
    # W604 backticks are deprecated, use 'repr()'
]

pep8_args = '--max-line-length=120'
pep8_warning = make_re('.')


def run(cmd, ignore_re, warning_re, prefix=''):
    output = commands.getoutput(cmd)
    for line in output.splitlines():
        if ignore_re and ignore_re.search(line):
            continue
        elif ': ' in line and warning_re.search(line):
            line = '%s: WARNING %s' % tuple(line.split(': ', 1))
        if line.count(':') == 3:
            # I found that flymake wasn't
            # recognizing these due to the column number, and my
            # attempts at hacking an elisp regex to match totally
            # failed.
            parts = line.split(':')
            parts.pop(2)
            line = ':'.join(parts)
        print prefix + line


run('pyflakes %s' % sys.argv[1], pyflakes_ignore, pyflakes_warning)
print '## pyflakes above, pep8 below ##'
pep8_ignore = ','.join(pep8_ignore)
run('pep8 --ignore=%s %s --repeat %s' % (pep8_ignore, pep8_args, sys.argv[1]), None, pep8_warning)

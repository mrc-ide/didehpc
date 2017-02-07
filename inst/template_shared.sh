#!/bin/sh
set -ex

# automatically generated
echo "generated on host: {{{hostname}}}"
echo "generated on date: {{{date}}}"
echo "didewin version: {{{didewin_version}}}"
echo "context version: {{{context_version}}}"
echo "running on: ${HOSTNAME}"
export CONTEXT_WORKDIR={{{context_workdir}}}
export CONTEXT_ROOT={{{context_root}}}
export CONTEXT_ID={{{context_id}}}
export CONTEXT_PROPAGATE_ERROR=TRUE

export PATH="/opt/local/R/{{{r_version}}}/bin:$PATH"

{{{#parallel}}}
echo This is a parallel job: will use $CPP_NUMCPUS
set CONTEXT_CORES=$CCP_NUMCPUS
{{{/parallel}}}

set REDIS_HOST={{{redis_host}}}
set REDIS_URL=redis://{{{redis_host}}}:6379

cd ${CONTEXT_WORKDIR}
echo "working directory: $PWD"

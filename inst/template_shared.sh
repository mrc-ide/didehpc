#!/bin/sh
set -e
## set -x # for verbose output

# automatically generated
echo "generated on host: {{{hostname}}}"
echo "generated on date: {{{date}}}"
echo "didehpc version: {{{didehpc_version}}}"
echo "context version: {{{context_version}}}"
echo "running on: ${HOSTNAME}"
export CONTEXT_WORKDIR={{{context_workdir}}}
export CONTEXT_ROOT={{{context_root}}}
export CONTEXT_ID={{{context_id}}}
export CONTEXT_PROPAGATE_ERROR=TRUE
export CONTEXT_BOOTSTRAP=TRUE

export PATH="/opt/local/R/{{{r_version}}}/bin:$PATH"

{{{#parallel}}}
echo This is a parallel job: will use $CPP_NUMCPUS
export CONTEXT_CORES=$CCP_NUMCPUS
{{{/parallel}}}

{{{#common_lib}}}
export R_LIBS={{{common_lib}}}
{{{/common_lib}}}

export REDIS_HOST={{{redis_host}}}
export REDIS_URL=redis://{{{redis_host}}}:6379

cd ${CONTEXT_WORKDIR}
echo "working directory: $PWD"

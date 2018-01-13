import { RECEIVE_SUMMARIES, RECEIVE_JOBS, RECEIVE_FAILURES, RECEIVE_JOB, RECEIVE_WORKERS } from "./Actions";

const JOB_ROUTE = "JOB_ROUTE";
const JOBS_ROUTE = "JOBS_ROUTE";
const QUEUE_SUMMARY_ROUTE = "QUEUE_SUMMARY_ROUTE";
const WORKERS_ROUTE = "WORKERS_ROUTE";
const FAILURES_ROUTE = "FAILURES_ROUTE";

const initialState = {
  last_updated: null,
  route: QUEUE_SUMMARY_ROUTE,
  summaries: [],
  jobs: [],
  failures: [],
  workers: [],
};

const reducer = (state, action) => {
  const now = Date.now();
  switch(action.type) {
  case RECEIVE_SUMMARIES:
    return { ...state, route: QUEUE_SUMMARY_ROUTE, summaries: action.summaries, last_updated: now };
  case RECEIVE_JOBS:
    return { ...state, route: JOBS_ROUTE, jobs: action.jobs, last_updated: now };
  case RECEIVE_FAILURES:
    return { ...state, route: FAILURES_ROUTE, failures: action.failures, last_updated: now };
  case RECEIVE_JOB:
    return {...state, route: JOB_ROUTE, job: action.job, last_updated: now };
  case RECEIVE_WORKERS:
    return {...state, route: WORKERS_ROUTE, workers: action.workers, last_updated: now };
  default:
    return initialState;
  }
};

export default reducer;

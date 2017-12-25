import _ from 'fetch';

export default {
  formatNumber(number) {
    return number.toFixed().replace(/\B(?=(\d{3})+(?!\d))/g, ',');
  },

  apiRequest(path, options) {
    options =  Object.assign({ credentials: 'include' }, options || {});
    return window.fetch(path, options).then(response => response.json());
  }
};


const POST = (path, payload) => {
    return fetch(path, {
        method: 'POST',
        headers: {
          'Accept': 'application/json',
          'Content-Type': 'application/json'
        },
        body: JSON.stringify(payload)
      }).then((res) => res.json());
};

export default {
    POST
}
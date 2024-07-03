export const writeStreamAndWait = async (stream, chunk) => {
    const { resolve, reject, promise } = Promise.withResolvers();
    stream.write(chunk, (error) => {
        if (error) {
            reject(error);
        } else {
            resolve();
        }
    });
    return promise;
};

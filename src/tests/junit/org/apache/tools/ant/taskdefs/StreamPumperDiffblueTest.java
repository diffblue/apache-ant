package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.util.concurrent.TimeUnit;
import org.apache.tools.ant.taskdefs.StreamPumper.PostStopHandle;
import org.junit.Test;

public class StreamPumperDiffblueTest {
  /**
   * Test getters and setters.
   * <ul>
   *   <li>When {@link ByteArrayInputStream#ByteArrayInputStream(byte[])} with {@code AXAXAXAX} Bytes is {@code UTF-8}.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link StreamPumper#StreamPumper(InputStream, OutputStream)}
   *   <li>{@link StreamPumper#setAutoflush(boolean)}
   *   <li>{@link StreamPumper#getBufferSize()}
   *   <li>{@link StreamPumper#getException()}
   *   <li>{@link StreamPumper#isFinished()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters_whenByteArrayInputStreamWithAxaxaxaxBytesIsUtf8()
      throws UnsupportedEncodingException {
    // Arrange
    ByteArrayInputStream is = new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"));

    // Act
    StreamPumper actualStreamPumper = new StreamPumper(is, new ByteArrayOutputStream(1));
    actualStreamPumper.setAutoflush(true);
    int actualBufferSize = actualStreamPumper.getBufferSize();
    Exception actualException = actualStreamPumper.getException();

    // Assert
    assertNull(actualException);
    assertEquals(128, actualBufferSize);
    assertFalse(actualStreamPumper.isFinished());
  }

  /**
   * Test getters and setters.
   * <ul>
   *   <li>When {@code true}.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link StreamPumper#StreamPumper(InputStream, OutputStream, boolean)}
   *   <li>{@link StreamPumper#setAutoflush(boolean)}
   *   <li>{@link StreamPumper#getBufferSize()}
   *   <li>{@link StreamPumper#getException()}
   *   <li>{@link StreamPumper#isFinished()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters_whenTrue() throws UnsupportedEncodingException {
    // Arrange
    ByteArrayInputStream is = new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"));

    // Act
    StreamPumper actualStreamPumper = new StreamPumper(is, new ByteArrayOutputStream(1), true);
    actualStreamPumper.setAutoflush(true);
    int actualBufferSize = actualStreamPumper.getBufferSize();
    Exception actualException = actualStreamPumper.getException();

    // Assert
    assertNull(actualException);
    assertEquals(128, actualBufferSize);
    assertFalse(actualStreamPumper.isFinished());
  }

  /**
   * Test getters and setters.
   * <ul>
   *   <li>When {@code true}.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link StreamPumper#StreamPumper(InputStream, OutputStream, boolean, boolean)}
   *   <li>{@link StreamPumper#setAutoflush(boolean)}
   *   <li>{@link StreamPumper#getBufferSize()}
   *   <li>{@link StreamPumper#getException()}
   *   <li>{@link StreamPumper#isFinished()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters_whenTrue2() throws UnsupportedEncodingException {
    // Arrange
    ByteArrayInputStream is = new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"));

    // Act
    StreamPumper actualStreamPumper = new StreamPumper(is, new ByteArrayOutputStream(1), true, true);
    actualStreamPumper.setAutoflush(true);
    int actualBufferSize = actualStreamPumper.getBufferSize();
    Exception actualException = actualStreamPumper.getException();

    // Assert
    assertNull(actualException);
    assertEquals(128, actualBufferSize);
    assertFalse(actualStreamPumper.isFinished());
  }

  /**
   * Test PostStopHandle {@link PostStopHandle#awaitPostStopCompletion(long, TimeUnit)}.
   * <p>
   * Method under test: {@link PostStopHandle#awaitPostStopCompletion(long, TimeUnit)}
   */
  @Test
  public void testPostStopHandleAwaitPostStopCompletion() throws UnsupportedEncodingException, InterruptedException {
    // Arrange
    ByteArrayInputStream is = new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"));

    // Act and Assert
    assertFalse(((new StreamPumper(is, new ByteArrayOutputStream(1))).new PostStopHandle()).awaitPostStopCompletion(10L,
        TimeUnit.NANOSECONDS));
  }

  /**
   * Test PostStopHandle {@link PostStopHandle#isInPostStopTasks()}.
   * <p>
   * Method under test: {@link PostStopHandle#isInPostStopTasks()}
   */
  @Test
  public void testPostStopHandleIsInPostStopTasks() throws UnsupportedEncodingException {
    // Arrange
    ByteArrayInputStream is = new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"));

    // Act and Assert
    assertTrue(((new StreamPumper(is, new ByteArrayOutputStream(1))).new PostStopHandle()).isInPostStopTasks());
  }

  /**
   * Test PostStopHandle {@link PostStopHandle#PostStopHandle(StreamPumper)}.
   * <p>
   * Method under test: {@link PostStopHandle#PostStopHandle(StreamPumper)}
   */
  @Test
  public void testPostStopHandleNewPostStopHandle() throws UnsupportedEncodingException {
    // Arrange
    ByteArrayInputStream is = new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"));

    // Act and Assert
    assertTrue(((new StreamPumper(is, new ByteArrayOutputStream(1))).new PostStopHandle()).isInPostStopTasks());
  }

  /**
   * Test {@link StreamPumper#run()}.
   * <p>
   * Method under test: {@link StreamPumper#run()}
   */
  @Test
  public void testRun() throws UnsupportedEncodingException {
    // Arrange
    ByteArrayInputStream is = new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"));
    StreamPumper streamPumper = new StreamPumper(is, new ByteArrayOutputStream(1));

    // Act
    streamPumper.run();

    // Assert
    assertTrue(streamPumper.isFinished());
  }

  /**
   * Test {@link StreamPumper#run()}.
   * <p>
   * Method under test: {@link StreamPumper#run()}
   */
  @Test
  public void testRun2() {
    // Arrange
    StreamPumper streamPumper = new StreamPumper(null, new ByteArrayOutputStream(1));

    // Act
    streamPumper.run();

    // Assert
    Exception exception = streamPumper.getException();
    assertEquals("Cannot invoke \"java.io.InputStream.read(byte[])\" because \"this.is\" is null",
        exception.getLocalizedMessage());
    assertEquals("Cannot invoke \"java.io.InputStream.read(byte[])\" because \"this.is\" is null",
        exception.getMessage());
    assertNull(exception.getCause());
    assertEquals(0, exception.getSuppressed().length);
  }

  /**
   * Test {@link StreamPumper#run()}.
   * <p>
   * Method under test: {@link StreamPumper#run()}
   */
  @Test
  public void testRun3() throws UnsupportedEncodingException {
    // Arrange
    StreamPumper streamPumper = new StreamPumper(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")), null);

    // Act
    streamPumper.run();

    // Assert
    Exception exception = streamPumper.getException();
    assertEquals("Cannot invoke \"java.io.OutputStream.write(byte[], int, int)\" because \"this.os\" is null",
        exception.getLocalizedMessage());
    assertEquals("Cannot invoke \"java.io.OutputStream.write(byte[], int, int)\" because \"this.os\" is null",
        exception.getMessage());
    assertNull(exception.getCause());
    assertEquals(0, exception.getSuppressed().length);
  }

  /**
   * Test {@link StreamPumper#run()}.
   * <p>
   * Method under test: {@link StreamPumper#run()}
   */
  @Test
  public void testRun4() {
    // Arrange
    ByteArrayInputStream is = new ByteArrayInputStream(new byte[]{'A', 1, 'A', 1, 'A', 1, 'A', 1});
    StreamPumper streamPumper = new StreamPumper(is, new ByteArrayOutputStream(1), true);

    // Act
    streamPumper.run();

    // Assert
    assertTrue(streamPumper.isFinished());
  }

  /**
   * Test {@link StreamPumper#setBufferSize(int)}.
   * <p>
   * Method under test: {@link StreamPumper#setBufferSize(int)}
   */
  @Test
  public void testSetBufferSize() throws UnsupportedEncodingException {
    // Arrange
    ByteArrayInputStream is = new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"));
    StreamPumper streamPumper = new StreamPumper(is, new ByteArrayOutputStream(1));

    // Act
    streamPumper.setBufferSize(3);

    // Assert
    assertEquals(3, streamPumper.getBufferSize());
  }

  /**
   * Test {@link StreamPumper#stop()}.
   * <p>
   * Method under test: {@link StreamPumper#stop()}
   */
  @Test
  public void testStop() throws UnsupportedEncodingException {
    // Arrange
    ByteArrayInputStream is = new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"));

    // Act and Assert
    assertTrue((new StreamPumper(is, new ByteArrayOutputStream(1))).stop().isInPostStopTasks());
  }
}

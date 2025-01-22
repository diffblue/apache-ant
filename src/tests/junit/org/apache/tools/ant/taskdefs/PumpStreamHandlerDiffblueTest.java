package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import org.apache.tools.ant.taskdefs.PumpStreamHandler.ThreadWithPumper;
import org.junit.Test;

public class PumpStreamHandlerDiffblueTest {
  /**
   * Test {@link PumpStreamHandler#PumpStreamHandler(OutputStream)}.
   * <ul>
   *   <li>Then return Err is {@link ByteArrayOutputStream#ByteArrayOutputStream(int)} with one.</li>
   * </ul>
   * <p>
   * Method under test: {@link PumpStreamHandler#PumpStreamHandler(OutputStream)}
   */
  @Test
  public void testNewPumpStreamHandler_thenReturnErrIsByteArrayOutputStreamWithOne() {
    // Arrange
    ByteArrayOutputStream outAndErr = new ByteArrayOutputStream(1);

    // Act
    PumpStreamHandler actualPumpStreamHandler = new PumpStreamHandler(outAndErr);

    // Assert
    assertSame(outAndErr, actualPumpStreamHandler.getErr());
    assertSame(outAndErr, actualPumpStreamHandler.getOut());
  }

  /**
   * Test {@link PumpStreamHandler#PumpStreamHandler(OutputStream, OutputStream)}.
   * <ul>
   *   <li>Then return Err is {@link ByteArrayOutputStream#ByteArrayOutputStream(int)} with one.</li>
   * </ul>
   * <p>
   * Method under test: {@link PumpStreamHandler#PumpStreamHandler(OutputStream, OutputStream)}
   */
  @Test
  public void testNewPumpStreamHandler_thenReturnErrIsByteArrayOutputStreamWithOne2() {
    // Arrange
    ByteArrayOutputStream out = new ByteArrayOutputStream(1);
    ByteArrayOutputStream err = new ByteArrayOutputStream(1);

    // Act
    PumpStreamHandler actualPumpStreamHandler = new PumpStreamHandler(out, err);

    // Assert
    assertSame(err, actualPumpStreamHandler.getErr());
    assertSame(out, actualPumpStreamHandler.getOut());
  }

  /**
   * Test {@link PumpStreamHandler#PumpStreamHandler(OutputStream, OutputStream, InputStream)}.
   * <ul>
   *   <li>Then return Err is {@link ByteArrayOutputStream#ByteArrayOutputStream(int)} with one.</li>
   * </ul>
   * <p>
   * Method under test: {@link PumpStreamHandler#PumpStreamHandler(OutputStream, OutputStream, InputStream)}
   */
  @Test
  public void testNewPumpStreamHandler_thenReturnErrIsByteArrayOutputStreamWithOne3()
      throws UnsupportedEncodingException {
    // Arrange
    ByteArrayOutputStream out = new ByteArrayOutputStream(1);
    ByteArrayOutputStream err = new ByteArrayOutputStream(1);

    // Act
    PumpStreamHandler actualPumpStreamHandler = new PumpStreamHandler(out, err,
        new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")));

    // Assert
    assertSame(err, actualPumpStreamHandler.getErr());
    assertSame(out, actualPumpStreamHandler.getOut());
  }

  /**
   * Test {@link PumpStreamHandler#PumpStreamHandler(OutputStream, OutputStream, InputStream, boolean)}.
   * <ul>
   *   <li>Then return Err is {@link ByteArrayOutputStream#ByteArrayOutputStream(int)} with one.</li>
   * </ul>
   * <p>
   * Method under test: {@link PumpStreamHandler#PumpStreamHandler(OutputStream, OutputStream, InputStream, boolean)}
   */
  @Test
  public void testNewPumpStreamHandler_thenReturnErrIsByteArrayOutputStreamWithOne4()
      throws UnsupportedEncodingException {
    // Arrange
    ByteArrayOutputStream out = new ByteArrayOutputStream(1);
    ByteArrayOutputStream err = new ByteArrayOutputStream(1);

    // Act
    PumpStreamHandler actualPumpStreamHandler = new PumpStreamHandler(out, err,
        new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")), true);

    // Assert
    assertSame(err, actualPumpStreamHandler.getErr());
    assertSame(out, actualPumpStreamHandler.getOut());
  }

  /**
   * Test {@link PumpStreamHandler#createPump(InputStream, OutputStream)} with {@code is}, {@code os}.
   * <p>
   * Method under test: {@link PumpStreamHandler#createPump(InputStream, OutputStream)}
   */
  @Test
  public void testCreatePumpWithIsOs() throws UnsupportedEncodingException {
    // Arrange
    PumpStreamHandler pumpStreamHandler = new PumpStreamHandler();
    ByteArrayInputStream is = new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"));

    // Act
    Thread actualCreatePumpResult = pumpStreamHandler.createPump(is, new ByteArrayOutputStream(1));

    // Assert
    assertTrue(actualCreatePumpResult instanceof ThreadWithPumper);
    StreamPumper pumper = ((ThreadWithPumper) actualCreatePumpResult).getPumper();
    assertNull(pumper.getException());
    assertEquals(128, pumper.getBufferSize());
    assertFalse(pumper.isFinished());
  }

  /**
   * Test {@link PumpStreamHandler#createPump(InputStream, OutputStream, boolean)} with {@code is}, {@code os}, {@code closeWhenExhausted}.
   * <p>
   * Method under test: {@link PumpStreamHandler#createPump(InputStream, OutputStream, boolean)}
   */
  @Test
  public void testCreatePumpWithIsOsCloseWhenExhausted() throws UnsupportedEncodingException {
    // Arrange
    PumpStreamHandler pumpStreamHandler = new PumpStreamHandler();
    ByteArrayInputStream is = new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"));

    // Act
    Thread actualCreatePumpResult = pumpStreamHandler.createPump(is, new ByteArrayOutputStream(1), true);

    // Assert
    assertTrue(actualCreatePumpResult instanceof ThreadWithPumper);
    StreamPumper pumper = ((ThreadWithPumper) actualCreatePumpResult).getPumper();
    assertNull(pumper.getException());
    assertEquals(128, pumper.getBufferSize());
    assertFalse(pumper.isFinished());
  }

  /**
   * Test {@link PumpStreamHandler#createPump(InputStream, OutputStream, boolean, boolean)} with {@code is}, {@code os}, {@code closeWhenExhausted}, {@code nonBlockingIO}.
   * <p>
   * Method under test: {@link PumpStreamHandler#createPump(InputStream, OutputStream, boolean, boolean)}
   */
  @Test
  public void testCreatePumpWithIsOsCloseWhenExhaustedNonBlockingIO() throws UnsupportedEncodingException {
    // Arrange
    PumpStreamHandler pumpStreamHandler = new PumpStreamHandler();
    ByteArrayInputStream is = new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"));

    // Act
    Thread actualCreatePumpResult = pumpStreamHandler.createPump(is, new ByteArrayOutputStream(1), true, true);

    // Assert
    assertTrue(actualCreatePumpResult instanceof ThreadWithPumper);
    StreamPumper pumper = ((ThreadWithPumper) actualCreatePumpResult).getPumper();
    assertNull(pumper.getException());
    assertEquals(128, pumper.getBufferSize());
    assertFalse(pumper.isFinished());
  }

  /**
   * Test ThreadWithPumper getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link ThreadWithPumper#ThreadWithPumper(StreamPumper)}
   *   <li>{@link ThreadWithPumper#getPumper()}
   * </ul>
   */
  @Test
  public void testThreadWithPumperGettersAndSetters() throws UnsupportedEncodingException {
    // Arrange
    ByteArrayInputStream is = new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"));
    StreamPumper p = new StreamPumper(is, new ByteArrayOutputStream(1));

    // Act and Assert
    assertSame(p, (new ThreadWithPumper(p)).getPumper());
  }
}

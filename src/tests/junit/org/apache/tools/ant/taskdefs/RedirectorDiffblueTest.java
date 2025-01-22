package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.util.Stack;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.ProjectComponent;
import org.apache.tools.ant.Task;
import org.apache.tools.ant.TaskAdapter;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.util.LazyFileOutputStream;
import org.apache.tools.ant.util.LineOrientedOutputStreamRedirector;
import org.apache.tools.ant.util.NullOutputStream;
import org.junit.Test;

public class RedirectorDiffblueTest {
  /**
   * Test {@link Redirector#Redirector(ProjectComponent)}.
   * <p>
   * Method under test: {@link Redirector#Redirector(ProjectComponent)}
   */
  @Test
  public void testNewRedirector() {
    // Arrange and Act
    Redirector actualRedirector = new Redirector(Path.systemBootClasspath);

    // Assert
    assertNull(actualRedirector.getInputStream());
    assertNull(actualRedirector.getErrorStream());
    assertNull(actualRedirector.getOutputStream());
  }

  /**
   * Test {@link Redirector#Redirector(Task)}.
   * <p>
   * Method under test: {@link Redirector#Redirector(Task)}
   */
  @Test
  public void testNewRedirector2() {
    // Arrange and Act
    Redirector actualRedirector = new Redirector(new TaskAdapter());

    // Assert
    assertNull(actualRedirector.getInputStream());
    assertNull(actualRedirector.getErrorStream());
    assertNull(actualRedirector.getOutputStream());
  }

  /**
   * Test {@link Redirector#setOutputEncoding(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Redirector#setOutputEncoding(String)}
   */
  @Test
  public void testSetOutputEncoding_whenNull_thenThrowIllegalArgumentException() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> (new Redirector(Path.systemBootClasspath)).setOutputEncoding(null));
  }

  /**
   * Test {@link Redirector#setErrorEncoding(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Redirector#setErrorEncoding(String)}
   */
  @Test
  public void testSetErrorEncoding_whenNull_thenThrowIllegalArgumentException() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> (new Redirector(Path.systemBootClasspath)).setErrorEncoding(null));
  }

  /**
   * Test {@link Redirector#setInputEncoding(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Redirector#setInputEncoding(String)}
   */
  @Test
  public void testSetInputEncoding_whenNull_thenThrowIllegalArgumentException() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> (new Redirector(Path.systemBootClasspath)).setInputEncoding(null));
  }

  /**
   * Test {@link Redirector#createStreams()}.
   * <p>
   * Method under test: {@link Redirector#createStreams()}
   */
  @Test
  public void testCreateStreams() {
    // Arrange
    Redirector redirector = new Redirector(Path.systemBootClasspath);
    redirector.setInput(new File[]{Copy.NULL_FILE_PLACEHOLDER});

    // Act
    redirector.createStreams();

    // Assert
    assertTrue(redirector.getErrorStream() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(redirector.getOutputStream() instanceof LineOrientedOutputStreamRedirector);
  }

  /**
   * Test {@link Redirector#createStreams()}.
   * <p>
   * Method under test: {@link Redirector#createStreams()}
   */
  @Test
  public void testCreateStreams2() throws IOException {
    // Arrange
    Redirector redirector = new Redirector(Path.systemBootClasspath);
    redirector.setInputString("Input String");

    // Act
    redirector.createStreams();

    // Assert
    assertTrue(redirector.getErrorStream() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(redirector.getOutputStream() instanceof LineOrientedOutputStreamRedirector);
    byte[] byteArray = new byte[12];
    assertEquals(12, redirector.getInputStream().read(byteArray));
    assertArrayEquals("Input String".getBytes("UTF-8"), byteArray);
  }

  /**
   * Test {@link Redirector#createStreams()}.
   * <p>
   * Method under test: {@link Redirector#createStreams()}
   */
  @Test
  public void testCreateStreams3() {
    // Arrange
    Redirector redirector = new Redirector(Path.systemBootClasspath);
    redirector.setOutput(new File[]{Copy.NULL_FILE_PLACEHOLDER});

    // Act
    redirector.createStreams();

    // Assert
    assertTrue(redirector.getErrorStream() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(redirector.getOutputStream() instanceof LineOrientedOutputStreamRedirector);
    assertNull(redirector.getInputStream());
  }

  /**
   * Test {@link Redirector#createStreams()}.
   * <p>
   * Method under test: {@link Redirector#createStreams()}
   */
  @Test
  public void testCreateStreams4() {
    // Arrange
    Redirector redirector = new Redirector(Path.systemBootClasspath);
    redirector.setLogError(true);

    // Act
    redirector.createStreams();

    // Assert
    OutputStream errorStream = redirector.getErrorStream();
    assertTrue(errorStream instanceof LogOutputStream);
    OutputStream outputStream = redirector.getOutputStream();
    assertTrue(outputStream instanceof LogOutputStream);
    assertEquals(1, ((LogOutputStream) errorStream).getMessageLevel());
    assertEquals(2, ((LogOutputStream) outputStream).getMessageLevel());
  }

  /**
   * Test {@link Redirector#createStreams()}.
   * <p>
   * Method under test: {@link Redirector#createStreams()}
   */
  @Test
  public void testCreateStreams5() {
    // Arrange
    Redirector redirector = new Redirector(Path.systemBootClasspath);
    redirector.setError(new File[]{Copy.NULL_FILE_PLACEHOLDER});

    // Act
    redirector.createStreams();

    // Assert
    OutputStream outputStream = redirector.getOutputStream();
    assertTrue(outputStream instanceof LogOutputStream);
    assertTrue(redirector.getErrorStream() instanceof LazyFileOutputStream);
    assertNull(redirector.getInputStream());
    assertEquals(2, ((LogOutputStream) outputStream).getMessageLevel());
  }

  /**
   * Test {@link Redirector#createStreams()}.
   * <p>
   * Method under test: {@link Redirector#createStreams()}
   */
  @Test
  public void testCreateStreams6() {
    // Arrange
    Redirector redirector = new Redirector(Path.systemBootClasspath);
    redirector.setDiscardOutput(true);

    // Act
    redirector.createStreams();

    // Assert
    assertTrue(redirector.getErrorStream() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(redirector.getOutputStream() instanceof LineOrientedOutputStreamRedirector);
    assertNull(redirector.getInputStream());
  }

  /**
   * Test {@link Redirector#createStreams()}.
   * <p>
   * Method under test: {@link Redirector#createStreams()}
   */
  @Test
  public void testCreateStreams7() {
    // Arrange
    Redirector redirector = new Redirector(Path.systemBootClasspath);
    redirector.setDiscardError(true);

    // Act
    redirector.createStreams();

    // Assert
    OutputStream outputStream = redirector.getOutputStream();
    assertTrue(outputStream instanceof LogOutputStream);
    assertTrue(redirector.getErrorStream() instanceof NullOutputStream);
    assertNull(redirector.getInputStream());
    assertEquals(2, ((LogOutputStream) outputStream).getMessageLevel());
  }

  /**
   * Test {@link Redirector#createStreams()}.
   * <p>
   * Method under test: {@link Redirector#createStreams()}
   */
  @Test
  public void testCreateStreams8() {
    // Arrange
    Redirector redirector = new Redirector(Path.systemBootClasspath);
    redirector.setOutputFilterChains(new Stack<>());

    // Act
    redirector.createStreams();

    // Assert
    assertTrue(redirector.getErrorStream() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(redirector.getOutputStream() instanceof LineOrientedOutputStreamRedirector);
    assertNull(redirector.getInputStream());
  }

  /**
   * Test {@link Redirector#createStreams()}.
   * <ul>
   *   <li>Given {@link Redirector#Redirector(ProjectComponent)} with managingTask is {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Redirector#createStreams()}
   */
  @Test
  public void testCreateStreams_givenRedirectorWithManagingTaskIsSystemBootClasspath() {
    // Arrange
    Redirector redirector = new Redirector(Path.systemBootClasspath);

    // Act
    redirector.createStreams();

    // Assert
    assertTrue(redirector.getErrorStream() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(redirector.getOutputStream() instanceof LineOrientedOutputStreamRedirector);
    assertNull(redirector.getInputStream());
  }

  /**
   * Test {@link Redirector#createHandler()}.
   * <p>
   * Method under test: {@link Redirector#createHandler()}
   */
  @Test
  public void testCreateHandler() throws BuildException {
    // Arrange
    Redirector redirector = new Redirector(Path.systemBootClasspath);
    redirector.setInput(new File[]{Copy.NULL_FILE_PLACEHOLDER});

    // Act
    ExecuteStreamHandler actualCreateHandlerResult = redirector.createHandler();

    // Assert
    assertTrue(actualCreateHandlerResult instanceof PumpStreamHandler);
    assertTrue(((PumpStreamHandler) actualCreateHandlerResult).getErr() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(((PumpStreamHandler) actualCreateHandlerResult).getOut() instanceof LineOrientedOutputStreamRedirector);
  }

  /**
   * Test {@link Redirector#createHandler()}.
   * <p>
   * Method under test: {@link Redirector#createHandler()}
   */
  @Test
  public void testCreateHandler2() throws IOException, BuildException {
    // Arrange
    Redirector redirector = new Redirector(Path.systemBootClasspath);
    redirector.setInputString("Input String");

    // Act
    ExecuteStreamHandler actualCreateHandlerResult = redirector.createHandler();

    // Assert
    assertTrue(actualCreateHandlerResult instanceof PumpStreamHandler);
    assertTrue(((PumpStreamHandler) actualCreateHandlerResult).getErr() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(((PumpStreamHandler) actualCreateHandlerResult).getOut() instanceof LineOrientedOutputStreamRedirector);
    byte[] byteArray = new byte[12];
    assertEquals(12, redirector.getInputStream().read(byteArray));
    assertArrayEquals("Input String".getBytes("UTF-8"), byteArray);
  }

  /**
   * Test {@link Redirector#createHandler()}.
   * <p>
   * Method under test: {@link Redirector#createHandler()}
   */
  @Test
  public void testCreateHandler3() throws BuildException {
    // Arrange
    Redirector redirector = new Redirector(Path.systemBootClasspath);
    redirector.setOutput(new File[]{Copy.NULL_FILE_PLACEHOLDER});

    // Act
    ExecuteStreamHandler actualCreateHandlerResult = redirector.createHandler();

    // Assert
    assertTrue(actualCreateHandlerResult instanceof PumpStreamHandler);
    assertTrue(((PumpStreamHandler) actualCreateHandlerResult).getErr() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(((PumpStreamHandler) actualCreateHandlerResult).getOut() instanceof LineOrientedOutputStreamRedirector);
    assertNull(redirector.getInputStream());
  }

  /**
   * Test {@link Redirector#createHandler()}.
   * <p>
   * Method under test: {@link Redirector#createHandler()}
   */
  @Test
  public void testCreateHandler4() throws BuildException {
    // Arrange
    Redirector redirector = new Redirector(Path.systemBootClasspath);
    redirector.setDiscardOutput(true);

    // Act
    ExecuteStreamHandler actualCreateHandlerResult = redirector.createHandler();

    // Assert
    assertTrue(actualCreateHandlerResult instanceof PumpStreamHandler);
    assertTrue(((PumpStreamHandler) actualCreateHandlerResult).getErr() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(((PumpStreamHandler) actualCreateHandlerResult).getOut() instanceof LineOrientedOutputStreamRedirector);
    assertNull(redirector.getInputStream());
  }

  /**
   * Test {@link Redirector#createHandler()}.
   * <p>
   * Method under test: {@link Redirector#createHandler()}
   */
  @Test
  public void testCreateHandler5() throws BuildException {
    // Arrange
    Redirector redirector = new Redirector(Path.systemBootClasspath);
    redirector.setOutputFilterChains(new Stack<>());

    // Act
    ExecuteStreamHandler actualCreateHandlerResult = redirector.createHandler();

    // Assert
    assertTrue(actualCreateHandlerResult instanceof PumpStreamHandler);
    assertTrue(((PumpStreamHandler) actualCreateHandlerResult).getErr() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(((PumpStreamHandler) actualCreateHandlerResult).getOut() instanceof LineOrientedOutputStreamRedirector);
    assertNull(redirector.getInputStream());
  }

  /**
   * Test {@link Redirector#createHandler()}.
   * <ul>
   *   <li>Given {@link Redirector#Redirector(ProjectComponent)} with managingTask is {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Redirector#createHandler()}
   */
  @Test
  public void testCreateHandler_givenRedirectorWithManagingTaskIsSystemBootClasspath() throws BuildException {
    // Arrange
    Redirector redirector = new Redirector(Path.systemBootClasspath);

    // Act
    ExecuteStreamHandler actualCreateHandlerResult = redirector.createHandler();

    // Assert
    assertTrue(actualCreateHandlerResult instanceof PumpStreamHandler);
    assertTrue(((PumpStreamHandler) actualCreateHandlerResult).getErr() instanceof LineOrientedOutputStreamRedirector);
    assertTrue(((PumpStreamHandler) actualCreateHandlerResult).getOut() instanceof LineOrientedOutputStreamRedirector);
    assertNull(redirector.getInputStream());
  }

  /**
   * Test {@link Redirector#createHandler()}.
   * <ul>
   *   <li>Then Err return {@link LazyFileOutputStream}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Redirector#createHandler()}
   */
  @Test
  public void testCreateHandler_thenErrReturnLazyFileOutputStream() throws BuildException {
    // Arrange
    Redirector redirector = new Redirector(Path.systemBootClasspath);
    redirector.setError(new File[]{Copy.NULL_FILE_PLACEHOLDER});

    // Act
    ExecuteStreamHandler actualCreateHandlerResult = redirector.createHandler();

    // Assert
    OutputStream out = ((PumpStreamHandler) actualCreateHandlerResult).getOut();
    assertTrue(out instanceof LogOutputStream);
    assertTrue(actualCreateHandlerResult instanceof PumpStreamHandler);
    assertTrue(((PumpStreamHandler) actualCreateHandlerResult).getErr() instanceof LazyFileOutputStream);
    assertNull(redirector.getInputStream());
    assertEquals(2, ((LogOutputStream) out).getMessageLevel());
  }

  /**
   * Test {@link Redirector#createHandler()}.
   * <ul>
   *   <li>Then Err return {@link LogOutputStream}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Redirector#createHandler()}
   */
  @Test
  public void testCreateHandler_thenErrReturnLogOutputStream() throws BuildException {
    // Arrange
    Redirector redirector = new Redirector(Path.systemBootClasspath);
    redirector.setLogError(true);

    // Act
    ExecuteStreamHandler actualCreateHandlerResult = redirector.createHandler();

    // Assert
    OutputStream err = ((PumpStreamHandler) actualCreateHandlerResult).getErr();
    assertTrue(err instanceof LogOutputStream);
    OutputStream out = ((PumpStreamHandler) actualCreateHandlerResult).getOut();
    assertTrue(out instanceof LogOutputStream);
    assertTrue(actualCreateHandlerResult instanceof PumpStreamHandler);
    assertEquals(1, ((LogOutputStream) err).getMessageLevel());
    assertEquals(2, ((LogOutputStream) out).getMessageLevel());
  }

  /**
   * Test {@link Redirector#createHandler()}.
   * <ul>
   *   <li>Then Err return {@link NullOutputStream}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Redirector#createHandler()}
   */
  @Test
  public void testCreateHandler_thenErrReturnNullOutputStream() throws BuildException {
    // Arrange
    Redirector redirector = new Redirector(Path.systemBootClasspath);
    redirector.setDiscardError(true);

    // Act
    ExecuteStreamHandler actualCreateHandlerResult = redirector.createHandler();

    // Assert
    OutputStream out = ((PumpStreamHandler) actualCreateHandlerResult).getOut();
    assertTrue(out instanceof LogOutputStream);
    assertTrue(actualCreateHandlerResult instanceof PumpStreamHandler);
    assertTrue(((PumpStreamHandler) actualCreateHandlerResult).getErr() instanceof NullOutputStream);
    assertNull(redirector.getInputStream());
    assertEquals(2, ((LogOutputStream) out).getMessageLevel());
  }

  /**
   * Test {@link Redirector#handleInput(byte[], int, int)}.
   * <ul>
   *   <li>Then return three.</li>
   * </ul>
   * <p>
   * Method under test: {@link Redirector#handleInput(byte[], int, int)}
   */
  @Test
  public void testHandleInput_thenReturnThree() throws IOException {
    // Arrange
    Redirector redirector = new Redirector(Path.systemBootClasspath);
    redirector.setInputStream(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")));

    // Act and Assert
    assertEquals(3, redirector.handleInput("AXAXAXAX".getBytes("UTF-8"), 2, 3));
    byte[] byteArray = new byte[5];
    assertEquals(5, redirector.getInputStream().read(byteArray));
    assertArrayEquals("XAXAX".getBytes("UTF-8"), byteArray);
  }
}

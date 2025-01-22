package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.junit.Test;

public class ExecuteDiffblueTest {
  /**
   * Test {@link Execute#toString(ByteArrayOutputStream)} with {@code ByteArrayOutputStream}.
   * <ul>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Execute#toString(ByteArrayOutputStream)}
   */
  @Test
  public void testToStringWithByteArrayOutputStream_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", Execute.toString(new ByteArrayOutputStream(1)));
  }

  /**
   * Test {@link Execute#Execute()}.
   * <p>
   * Method under test: {@link Execute#Execute()}
   */
  @Test
  public void testNewExecute() {
    // Arrange and Act
    Execute actualExecute = new Execute();

    // Assert
    File workingDirectory = actualExecute.getWorkingDirectory();
    assertEquals("apache-ant-1.10.15", workingDirectory.getName());
    assertNull(actualExecute.getCommandline());
    assertNull(actualExecute.getEnvironment());
    assertTrue(workingDirectory.isAbsolute());
    assertTrue(actualExecute.isFailure());
    assertEquals(Execute.INVALID, actualExecute.getExitValue());
  }

  /**
   * Test {@link Execute#Execute(ExecuteStreamHandler)}.
   * <p>
   * Method under test: {@link Execute#Execute(ExecuteStreamHandler)}
   */
  @Test
  public void testNewExecute2() {
    // Arrange and Act
    Execute actualExecute = new Execute(new PumpStreamHandler());

    // Assert
    File workingDirectory = actualExecute.getWorkingDirectory();
    assertEquals("apache-ant-1.10.15", workingDirectory.getName());
    assertNull(actualExecute.getCommandline());
    assertNull(actualExecute.getEnvironment());
    assertTrue(workingDirectory.isAbsolute());
    assertTrue(actualExecute.isFailure());
    assertEquals(Execute.INVALID, actualExecute.getExitValue());
  }

  /**
   * Test {@link Execute#Execute(ExecuteStreamHandler, ExecuteWatchdog)}.
   * <p>
   * Method under test: {@link Execute#Execute(ExecuteStreamHandler, ExecuteWatchdog)}
   */
  @Test
  public void testNewExecute3() {
    // Arrange
    PumpStreamHandler streamHandler = new PumpStreamHandler();

    // Act
    Execute actualExecute = new Execute(streamHandler, new ExecuteWatchdog(10));

    // Assert
    File workingDirectory = actualExecute.getWorkingDirectory();
    assertEquals("apache-ant-1.10.15", workingDirectory.getName());
    assertNull(actualExecute.getCommandline());
    assertNull(actualExecute.getEnvironment());
    assertTrue(workingDirectory.isAbsolute());
    assertTrue(actualExecute.isFailure());
    assertEquals(Execute.INVALID, actualExecute.getExitValue());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Execute#setAntRun(Project)}
   *   <li>{@link Execute#setCommandline(String[])}
   *   <li>{@link Execute#setEnvironment(String[])}
   *   <li>{@link Execute#setExitValue(int)}
   *   <li>{@link Execute#setNewenvironment(boolean)}
   *   <li>{@link Execute#setStreamHandler(ExecuteStreamHandler)}
   *   <li>{@link Execute#setVMLauncher(boolean)}
   *   <li>{@link Execute#setWorkingDirectory(File)}
   *   <li>{@link Execute#setSpawn(boolean)}
   *   <li>{@link Execute#getCommandline()}
   *   <li>{@link Execute#getExitValue()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() throws BuildException {
    // Arrange
    Execute execute = new Execute();

    // Act
    execute.setAntRun(new Project());
    String[] commandline = new String[]{"Commandline"};
    execute.setCommandline(commandline);
    execute.setEnvironment(new String[]{"Env"});
    execute.setExitValue(42);
    execute.setNewenvironment(true);
    execute.setStreamHandler(new PumpStreamHandler());
    execute.setVMLauncher(true);
    execute.setWorkingDirectory(Copy.NULL_FILE_PLACEHOLDER);
    execute.setSpawn(true);
    String[] actualCommandline = execute.getCommandline();

    // Assert
    assertEquals(42, execute.getExitValue());
    assertSame(commandline, actualCommandline);
    assertArrayEquals(new String[]{"Commandline"}, actualCommandline);
  }

  /**
   * Test {@link Execute#getEnvironment()}.
   * <ul>
   *   <li>Given {@link Execute#Execute()}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Execute#getEnvironment()}
   */
  @Test
  public void testGetEnvironment_givenExecute_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull((new Execute()).getEnvironment());
  }

  /**
   * Test {@link Execute#getEnvironment()}.
   * <ul>
   *   <li>Then return array of {@link String} with {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Execute#getEnvironment()}
   */
  @Test
  public void testGetEnvironment_thenReturnArrayOfStringWithFoo() {
    // Arrange
    Execute execute = new Execute();
    execute.setEnvironment(new String[]{"foo"});
    execute.setNewenvironment(true);

    // Act and Assert
    assertArrayEquals(new String[]{"foo"}, execute.getEnvironment());
  }

  /**
   * Test {@link Execute#getWorkingDirectory()}.
   * <ul>
   *   <li>Given {@link Execute#Execute()}.</li>
   *   <li>Then return Name is {@code apache-ant-1.10.15}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Execute#getWorkingDirectory()}
   */
  @Test
  public void testGetWorkingDirectory_givenExecute_thenReturnNameIsApacheAnt11015() {
    // Arrange and Act
    File actualWorkingDirectory = (new Execute()).getWorkingDirectory();

    // Assert
    assertEquals("apache-ant-1.10.15", actualWorkingDirectory.getName());
    assertTrue(actualWorkingDirectory.isAbsolute());
  }

  /**
   * Test {@link Execute#getWorkingDirectory()}.
   * <ul>
   *   <li>Then return Name is {@code NULL_FILE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Execute#getWorkingDirectory()}
   */
  @Test
  public void testGetWorkingDirectory_thenReturnNameIsNullFile() {
    // Arrange
    Execute execute = new Execute();
    execute.setWorkingDirectory(Copy.NULL_FILE_PLACEHOLDER);

    // Act
    File actualWorkingDirectory = execute.getWorkingDirectory();

    // Assert
    assertEquals("NULL_FILE", actualWorkingDirectory.getName());
    assertTrue(actualWorkingDirectory.isAbsolute());
  }

  /**
   * Test {@link Execute#launch(Project, String[], String[], File, boolean)}.
   * <ul>
   *   <li>When {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Execute#launch(Project, String[], String[], File, boolean)}
   */
  @Test
  public void testLaunch_whenNull_file_placeholder_thenThrowBuildException() throws IOException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> Execute.launch(new Project(), new String[]{"Command"}, new String[]{"Env"},
        Copy.NULL_FILE_PLACEHOLDER, true));
  }

  /**
   * Test {@link Execute#execute()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Execute#execute()}
   */
  @Test
  public void testExecute_thenThrowBuildException() throws IOException {
    // Arrange
    Execute execute = new Execute();
    execute.setWorkingDirectory(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertThrows(BuildException.class, () -> execute.execute());
  }

  /**
   * Test {@link Execute#spawn()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Execute#spawn()}
   */
  @Test
  public void testSpawn_thenThrowBuildException() throws IOException {
    // Arrange
    Execute execute = new Execute();
    execute.setWorkingDirectory(Copy.NULL_FILE_PLACEHOLDER);
    execute.setEnvironment(null);
    execute.setNewenvironment(false);
    execute.setVMLauncher(false);

    // Act and Assert
    assertThrows(BuildException.class, () -> execute.spawn());
  }

  /**
   * Test {@link Execute#isFailure(int)} with {@code int}.
   * <ul>
   *   <li>When forty-two.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Execute#isFailure(int)}
   */
  @Test
  public void testIsFailureWithInt_whenFortyTwo_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(Execute.isFailure(42));
  }

  /**
   * Test {@link Execute#isFailure(int)} with {@code int}.
   * <ul>
   *   <li>When zero.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Execute#isFailure(int)}
   */
  @Test
  public void testIsFailureWithInt_whenZero_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(Execute.isFailure(0));
  }

  /**
   * Test {@link Execute#isFailure()}.
   * <ul>
   *   <li>Given {@link Execute#Execute()} ExitValue is zero.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Execute#isFailure()}
   */
  @Test
  public void testIsFailure_givenExecuteExitValueIsZero_thenReturnFalse() {
    // Arrange
    Execute execute = new Execute();
    execute.setExitValue(0);

    // Act and Assert
    assertFalse(execute.isFailure());
  }

  /**
   * Test {@link Execute#isFailure()}.
   * <ul>
   *   <li>Given {@link Execute#Execute()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Execute#isFailure()}
   */
  @Test
  public void testIsFailure_givenExecute_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new Execute()).isFailure());
  }

  /**
   * Test {@link Execute#killedProcess()}.
   * <ul>
   *   <li>Given {@link Execute#Execute()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Execute#killedProcess()}
   */
  @Test
  public void testKilledProcess_givenExecute() {
    // Arrange, Act and Assert
    assertFalse((new Execute()).killedProcess());
  }

  /**
   * Test {@link Execute#killedProcess()}.
   * <ul>
   *   <li>Given {@link ExecuteWatchdog#ExecuteWatchdog(int)} with timeout is ten.</li>
   * </ul>
   * <p>
   * Method under test: {@link Execute#killedProcess()}
   */
  @Test
  public void testKilledProcess_givenExecuteWatchdogWithTimeoutIsTen() {
    // Arrange
    PumpStreamHandler streamHandler = new PumpStreamHandler();

    // Act and Assert
    assertFalse((new Execute(streamHandler, new ExecuteWatchdog(10))).killedProcess());
  }
}

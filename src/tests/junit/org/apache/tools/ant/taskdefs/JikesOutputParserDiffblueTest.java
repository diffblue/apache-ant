package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.CharArrayReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.StringReader;
import java.io.UnsupportedEncodingException;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Task;
import org.apache.tools.ant.TaskAdapter;
import org.junit.Test;

public class JikesOutputParserDiffblueTest {
  /**
   * Test {@link JikesOutputParser#start()}.
   * <p>
   * Method under test: {@link JikesOutputParser#start()}
   */
  @Test
  public void testStart() throws IOException {
    // Arrange
    JikesOutputParser jikesOutputParser = new JikesOutputParser(new TaskAdapter(), true);
    jikesOutputParser.setProcessOutputStream(new ByteArrayInputStream(new byte[]{'A', 1, 'A', 1, 'A', 1, 'A', 1}));

    // Act
    jikesOutputParser.start();

    // Assert
    Stream<String> linesResult = jikesOutputParser.br.lines();
    assertEquals("", linesResult.collect(Collectors.joining("\n")));
    assertTrue(jikesOutputParser.getErrorFlag());
    assertTrue(jikesOutputParser.error);
  }

  /**
   * Test {@link JikesOutputParser#start()}.
   * <p>
   * Method under test: {@link JikesOutputParser#start()}
   */
  @Test
  public void testStart2() throws IOException {
    // Arrange
    JikesOutputParser jikesOutputParser = new JikesOutputParser(new TaskAdapter(), false);
    jikesOutputParser.setProcessOutputStream(new ByteArrayInputStream(new byte[]{'A', 1, 'A', 1, 'A', 1, 'A', 1}));

    // Act
    jikesOutputParser.start();

    // Assert
    Stream<String> linesResult = jikesOutputParser.br.lines();
    assertEquals("", linesResult.collect(Collectors.joining("\n")));
    assertFalse(jikesOutputParser.getErrorFlag());
    assertFalse(jikesOutputParser.error);
  }

  /**
   * Test {@link JikesOutputParser#start()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JikesOutputParser#start()}
   */
  @Test
  public void testStart_givenJavaLangObject() throws IOException {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("warning", typeClass);
    project.addBuildListener(new AntClassLoader());

    TaskAdapter task = new TaskAdapter();
    task.setProject(project);

    JikesOutputParser jikesOutputParser = new JikesOutputParser(task, true);
    jikesOutputParser.setProcessOutputStream(new ByteArrayInputStream(new byte[]{'A', 1, 'A', 1, 'A', 1, 'A', 1}));

    // Act
    jikesOutputParser.start();

    // Assert
    Stream<String> linesResult = jikesOutputParser.br.lines();
    assertEquals("", linesResult.collect(Collectors.joining("\n")));
    assertTrue(jikesOutputParser.getErrorFlag());
    assertTrue(jikesOutputParser.error);
  }

  /**
   * Test {@link JikesOutputParser#start()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JikesOutputParser#start()}
   */
  @Test
  public void testStart_givenProjectAddBuildListenerAntClassLoader() throws IOException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    TaskAdapter task = new TaskAdapter();
    task.setProject(project);

    JikesOutputParser jikesOutputParser = new JikesOutputParser(task, true);
    jikesOutputParser.setProcessOutputStream(new ByteArrayInputStream(new byte[]{'A', 1, 'A', 1, 'A', 1, 'A', 1}));

    // Act
    jikesOutputParser.start();

    // Assert
    Stream<String> linesResult = jikesOutputParser.br.lines();
    assertEquals("", linesResult.collect(Collectors.joining("\n")));
    assertTrue(jikesOutputParser.getErrorFlag());
    assertTrue(jikesOutputParser.error);
  }

  /**
   * Test {@link JikesOutputParser#start()}.
   * <ul>
   *   <li>Given {@link TaskAdapter#TaskAdapter()} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link JikesOutputParser#start()}
   */
  @Test
  public void testStart_givenTaskAdapterProjectIsProject() throws IOException {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    task.setProject(new Project());

    JikesOutputParser jikesOutputParser = new JikesOutputParser(task, true);
    jikesOutputParser.setProcessOutputStream(new ByteArrayInputStream(new byte[]{'A', 1, 'A', 1, 'A', 1, 'A', 1}));

    // Act
    jikesOutputParser.start();

    // Assert
    Stream<String> linesResult = jikesOutputParser.br.lines();
    assertEquals("", linesResult.collect(Collectors.joining("\n")));
    assertTrue(jikesOutputParser.getErrorFlag());
    assertTrue(jikesOutputParser.error);
  }

  /**
   * Test {@link JikesOutputParser#JikesOutputParser(Task, boolean)}.
   * <p>
   * Method under test: {@link JikesOutputParser#JikesOutputParser(Task, boolean)}
   */
  @Test
  public void testNewJikesOutputParser() {
    // Arrange and Act
    JikesOutputParser actualJikesOutputParser = new JikesOutputParser(new TaskAdapter(), true);

    // Assert
    Task task = actualJikesOutputParser.task;
    assertTrue(task instanceof TaskAdapter);
    assertNull(actualJikesOutputParser.br);
    assertNull(((TaskAdapter) task).getProxy());
    assertNull(task.getDescription());
    assertNull(task.getTaskName());
    assertNull(task.getTaskType());
    assertNull(task.getProject());
    assertNull(task.getOwningTarget());
    assertEquals(0, actualJikesOutputParser.errors);
    assertEquals(0, actualJikesOutputParser.warnings);
    assertFalse(actualJikesOutputParser.getErrorFlag());
    assertFalse(actualJikesOutputParser.error);
    assertTrue(actualJikesOutputParser.emacsMode);
  }

  /**
   * Test {@link JikesOutputParser#parseOutput(BufferedReader)}.
   * <p>
   * Method under test: {@link JikesOutputParser#parseOutput(BufferedReader)}
   */
  @Test
  public void testParseOutput() throws IOException {
    // Arrange
    JikesOutputParser jikesOutputParser = new JikesOutputParser(new TaskAdapter(), true);

    // Act
    jikesOutputParser.parseOutput(new BufferedReader(new StringReader("foo"), 1));

    // Assert
    assertTrue(jikesOutputParser.getErrorFlag());
    assertTrue(jikesOutputParser.error);
  }

  /**
   * Test {@link JikesOutputParser#parseOutput(BufferedReader)}.
   * <p>
   * Method under test: {@link JikesOutputParser#parseOutput(BufferedReader)}
   */
  @Test
  public void testParseOutput2() throws IOException {
    // Arrange
    JikesOutputParser jikesOutputParser = new JikesOutputParser(new TaskAdapter(), false);

    // Act
    jikesOutputParser.parseOutput(new BufferedReader(new StringReader("foo"), 1));

    // Assert that nothing has changed
    assertFalse(jikesOutputParser.getErrorFlag());
    assertFalse(jikesOutputParser.error);
  }

  /**
   * Test {@link JikesOutputParser#parseOutput(BufferedReader)}.
   * <p>
   * Method under test: {@link JikesOutputParser#parseOutput(BufferedReader)}
   */
  @Test
  public void testParseOutput3() throws IOException {
    // Arrange
    JikesOutputParser jikesOutputParser = new JikesOutputParser(new TaskAdapter(), true);

    // Act
    jikesOutputParser.parseOutput(new BufferedReader(new CharArrayReader("\u0001\u0002\u0001\u0002".toCharArray()), 1));

    // Assert that nothing has changed
    assertFalse(jikesOutputParser.getErrorFlag());
    assertFalse(jikesOutputParser.error);
  }

  /**
   * Test {@link JikesOutputParser#parseOutput(BufferedReader)}.
   * <p>
   * Method under test: {@link JikesOutputParser#parseOutput(BufferedReader)}
   */
  @Test
  public void testParseOutput4() throws IOException {
    // Arrange
    JikesOutputParser jikesOutputParser = new JikesOutputParser(new TaskAdapter(), false);

    // Act
    jikesOutputParser.parseOutput(new BufferedReader(new StringReader("error"), 1));

    // Assert
    assertTrue(jikesOutputParser.getErrorFlag());
    assertTrue(jikesOutputParser.error);
  }

  /**
   * Test {@link JikesOutputParser#parseOutput(BufferedReader)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JikesOutputParser#parseOutput(BufferedReader)}
   */
  @Test
  public void testParseOutput_givenJavaLangObject() throws IOException {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("warning", typeClass);
    project.addBuildListener(new AntClassLoader());

    TaskAdapter task = new TaskAdapter();
    task.setProject(project);
    JikesOutputParser jikesOutputParser = new JikesOutputParser(task, true);

    // Act
    jikesOutputParser.parseOutput(new BufferedReader(new StringReader("foo"), 1));

    // Assert
    assertTrue(jikesOutputParser.getErrorFlag());
    assertTrue(jikesOutputParser.error);
  }

  /**
   * Test {@link JikesOutputParser#parseOutput(BufferedReader)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JikesOutputParser#parseOutput(BufferedReader)}
   */
  @Test
  public void testParseOutput_givenProjectAddBuildListenerAntClassLoader() throws IOException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    TaskAdapter task = new TaskAdapter();
    task.setProject(project);
    JikesOutputParser jikesOutputParser = new JikesOutputParser(task, true);

    // Act
    jikesOutputParser.parseOutput(new BufferedReader(new StringReader("foo"), 1));

    // Assert
    assertTrue(jikesOutputParser.getErrorFlag());
    assertTrue(jikesOutputParser.error);
  }

  /**
   * Test {@link JikesOutputParser#parseOutput(BufferedReader)}.
   * <ul>
   *   <li>Given {@link TaskAdapter#TaskAdapter()} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link JikesOutputParser#parseOutput(BufferedReader)}
   */
  @Test
  public void testParseOutput_givenTaskAdapterProjectIsProject() throws IOException {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    task.setProject(new Project());
    JikesOutputParser jikesOutputParser = new JikesOutputParser(task, true);

    // Act
    jikesOutputParser.parseOutput(new BufferedReader(new StringReader("foo"), 1));

    // Assert
    assertTrue(jikesOutputParser.getErrorFlag());
    assertTrue(jikesOutputParser.error);
  }

  /**
   * Test {@link JikesOutputParser#parseOutput(BufferedReader)}.
   * <ul>
   *   <li>When {@link StringReader#StringReader(String)} with {@code error}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JikesOutputParser#parseOutput(BufferedReader)}
   */
  @Test
  public void testParseOutput_whenStringReaderWithError() throws IOException {
    // Arrange
    JikesOutputParser jikesOutputParser = new JikesOutputParser(new TaskAdapter(), true);

    // Act
    jikesOutputParser.parseOutput(new BufferedReader(new StringReader("error"), 1));

    // Assert
    assertTrue(jikesOutputParser.getErrorFlag());
    assertTrue(jikesOutputParser.error);
  }

  /**
   * Test {@link JikesOutputParser#parseOutput(BufferedReader)}.
   * <ul>
   *   <li>When {@link StringReader#StringReader(String)} with {@code warning}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JikesOutputParser#parseOutput(BufferedReader)}
   */
  @Test
  public void testParseOutput_whenStringReaderWithWarning() throws IOException {
    // Arrange
    JikesOutputParser jikesOutputParser = new JikesOutputParser(new TaskAdapter(), true);

    // Act
    jikesOutputParser.parseOutput(new BufferedReader(new StringReader("warning"), 1));

    // Assert that nothing has changed
    assertFalse(jikesOutputParser.getErrorFlag());
    assertFalse(jikesOutputParser.error);
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link JikesOutputParser#setProcessErrorStream(InputStream)}
   *   <li>{@link JikesOutputParser#setProcessInputStream(OutputStream)}
   *   <li>{@link JikesOutputParser#stop()}
   *   <li>{@link JikesOutputParser#getErrorFlag()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() throws UnsupportedEncodingException {
    // Arrange
    JikesOutputParser jikesOutputParser = new JikesOutputParser(new TaskAdapter(), true);

    // Act
    jikesOutputParser.setProcessErrorStream(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")));
    jikesOutputParser.setProcessInputStream(new ByteArrayOutputStream(1));
    jikesOutputParser.stop();

    // Assert
    assertFalse(jikesOutputParser.getErrorFlag());
  }
}

package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.util.Vector;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildEvent;
import org.apache.tools.ant.BuildListener;
import org.apache.tools.ant.Project;
import org.junit.Test;

public class RecorderEntryDiffblueTest {
  /**
   * Test {@link RecorderEntry#RecorderEntry(String)}.
   * <p>
   * Method under test: {@link RecorderEntry#RecorderEntry(String)}
   */
  @Test
  public void testNewRecorderEntry() {
    // Arrange and Act
    RecorderEntry actualRecorderEntry = new RecorderEntry(Manifest.ATTRIBUTE_NAME);

    // Assert
    assertNull(actualRecorderEntry.getProject());
    assertEquals(2, actualRecorderEntry.getMessageOutputLevel());
    assertEquals(Manifest.ATTRIBUTE_NAME, actualRecorderEntry.getFilename());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link RecorderEntry#setEmacsMode(boolean)}
   *   <li>{@link RecorderEntry#subBuildStarted(BuildEvent)}
   *   <li>{@link RecorderEntry#getFilename()}
   *   <li>{@link RecorderEntry#getMessageOutputLevel()}
   *   <li>{@link RecorderEntry#getProject()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    RecorderEntry recorderEntry = new RecorderEntry(Manifest.ATTRIBUTE_NAME);

    // Act
    recorderEntry.setEmacsMode(true);
    recorderEntry.subBuildStarted(new BuildEvent(new Project()));
    String actualFilename = recorderEntry.getFilename();
    int actualMessageOutputLevel = recorderEntry.getMessageOutputLevel();

    // Assert
    assertNull(recorderEntry.getProject());
    assertEquals(2, actualMessageOutputLevel);
    assertEquals(Manifest.ATTRIBUTE_NAME, actualFilename);
  }

  /**
   * Test {@link RecorderEntry#buildFinished(BuildEvent)}.
   * <ul>
   *   <li>Given {@link ByteArrayOutputStream#ByteArrayOutputStream(int)} with one.</li>
   * </ul>
   * <p>
   * Method under test: {@link RecorderEntry#buildFinished(BuildEvent)}
   */
  @Test
  public void testBuildFinished_givenByteArrayOutputStreamWithOne() {
    // Arrange
    RecorderEntry recorderEntry = new RecorderEntry(Manifest.ATTRIBUTE_NAME);
    recorderEntry.setRecordState(false);
    recorderEntry.setOutputPrintStream(new PrintStream(new ByteArrayOutputStream(1)));
    recorderEntry.setMessageOutputLevel(4);
    recorderEntry.setProject(null);

    BuildEvent event = new BuildEvent(new Project());
    event.setException(null);

    // Act
    recorderEntry.buildFinished(event);

    // Assert that nothing has changed
    assertNull(recorderEntry.getProject());
  }

  /**
   * Test {@link RecorderEntry#buildFinished(BuildEvent)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RecorderEntry#buildFinished(BuildEvent)}
   */
  @Test
  public void testBuildFinished_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    RecorderEntry recorderEntry = new RecorderEntry(Manifest.ATTRIBUTE_NAME);
    recorderEntry.setRecordState(false);
    recorderEntry.setOutputPrintStream(null);
    recorderEntry.setMessageOutputLevel(4);
    recorderEntry.setProject(project);

    BuildEvent event = new BuildEvent(new Project());
    event.setException(null);

    // Act
    recorderEntry.buildFinished(event);

    // Assert
    assertNull(recorderEntry.getProject());
  }

  /**
   * Test {@link RecorderEntry#buildFinished(BuildEvent)}.
   * <ul>
   *   <li>Given {@link RecorderEntry#RecorderEntry(String)} with name is {@link Manifest#ATTRIBUTE_NAME}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RecorderEntry#buildFinished(BuildEvent)}
   */
  @Test
  public void testBuildFinished_givenRecorderEntryWithNameIsAttribute_name() {
    // Arrange
    RecorderEntry recorderEntry = new RecorderEntry(Manifest.ATTRIBUTE_NAME);

    // Act
    recorderEntry.buildFinished(new BuildEvent(new Project()));

    // Assert that nothing has changed
    assertNull(recorderEntry.getProject());
  }

  /**
   * Test {@link RecorderEntry#buildFinished(BuildEvent)}.
   * <ul>
   *   <li>Given {@link RecorderEntry#RecorderEntry(String)} with name is {@link Manifest#ATTRIBUTE_NAME} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link RecorderEntry#buildFinished(BuildEvent)}
   */
  @Test
  public void testBuildFinished_givenRecorderEntryWithNameIsAttribute_nameProjectIsProject() {
    // Arrange
    RecorderEntry recorderEntry = new RecorderEntry(Manifest.ATTRIBUTE_NAME);
    recorderEntry.setRecordState(false);
    recorderEntry.setOutputPrintStream(null);
    recorderEntry.setMessageOutputLevel(4);
    recorderEntry.setProject(new Project());

    BuildEvent event = new BuildEvent(new Project());
    event.setException(null);

    // Act
    recorderEntry.buildFinished(event);

    // Assert
    assertNull(recorderEntry.getProject());
  }

  /**
   * Test {@link RecorderEntry#buildFinished(BuildEvent)}.
   * <ul>
   *   <li>Given {@link RecorderEntry#RecorderEntry(String)} with name is {@link Manifest#ATTRIBUTE_NAME} RecordState is {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RecorderEntry#buildFinished(BuildEvent)}
   */
  @Test
  public void testBuildFinished_givenRecorderEntryWithNameIsAttribute_nameRecordStateIsFalse() {
    // Arrange
    RecorderEntry recorderEntry = new RecorderEntry(Manifest.ATTRIBUTE_NAME);
    recorderEntry.setRecordState(false);
    recorderEntry.setOutputPrintStream(null);
    recorderEntry.setMessageOutputLevel(4);
    recorderEntry.setProject(null);

    BuildEvent event = new BuildEvent(new Project());
    event.setException(null);

    // Act
    recorderEntry.buildFinished(event);

    // Assert that nothing has changed
    assertNull(recorderEntry.getProject());
  }

  /**
   * Test {@link RecorderEntry#buildFinished(BuildEvent)}.
   * <ul>
   *   <li>Given {@link RecorderEntry#RecorderEntry(String)} with name is {@link Manifest#ATTRIBUTE_NAME} RecordState is {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RecorderEntry#buildFinished(BuildEvent)}
   */
  @Test
  public void testBuildFinished_givenRecorderEntryWithNameIsAttribute_nameRecordStateIsTrue() {
    // Arrange
    RecorderEntry recorderEntry = new RecorderEntry(Manifest.ATTRIBUTE_NAME);
    recorderEntry.setRecordState(true);
    recorderEntry.setOutputPrintStream(null);
    recorderEntry.setMessageOutputLevel(4);
    recorderEntry.setProject(null);

    BuildEvent event = new BuildEvent(new Project());
    event.setException(null);

    // Act
    recorderEntry.buildFinished(event);

    // Assert that nothing has changed
    assertNull(recorderEntry.getProject());
  }

  /**
   * Test {@link RecorderEntry#buildFinished(BuildEvent)}.
   * <ul>
   *   <li>Given {@link RecorderEntry#RecorderEntry(String)} with name is {@link Manifest#ATTRIBUTE_NAME} RecordState is {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RecorderEntry#buildFinished(BuildEvent)}
   */
  @Test
  public void testBuildFinished_givenRecorderEntryWithNameIsAttribute_nameRecordStateIsTrue2() {
    // Arrange
    RecorderEntry recorderEntry = new RecorderEntry(Manifest.ATTRIBUTE_NAME);
    recorderEntry.setRecordState(true);
    recorderEntry.setOutputPrintStream(new PrintStream(new ByteArrayOutputStream(1)));
    recorderEntry.setMessageOutputLevel(4);
    recorderEntry.setProject(null);

    BuildEvent event = new BuildEvent(new Project());
    event.setException(null);

    // Act
    recorderEntry.buildFinished(event);

    // Assert that nothing has changed
    assertNull(recorderEntry.getProject());
  }

  /**
   * Test {@link RecorderEntry#buildFinished(BuildEvent)}.
   * <ul>
   *   <li>Given {@link Throwable#Throwable()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RecorderEntry#buildFinished(BuildEvent)}
   */
  @Test
  public void testBuildFinished_givenThrowable() {
    // Arrange
    RecorderEntry recorderEntry = new RecorderEntry(Manifest.ATTRIBUTE_NAME);
    recorderEntry.setRecordState(true);
    recorderEntry.setOutputPrintStream(new PrintStream(new ByteArrayOutputStream(1)));
    recorderEntry.setMessageOutputLevel(4);
    recorderEntry.setProject(null);

    BuildEvent event = new BuildEvent(new Project());
    event.setException(new Throwable());

    // Act
    recorderEntry.buildFinished(event);

    // Assert that nothing has changed
    assertNull(recorderEntry.getProject());
  }

  /**
   * Test {@link RecorderEntry#setMessageOutputLevel(int)}.
   * <p>
   * Method under test: {@link RecorderEntry#setMessageOutputLevel(int)}
   */
  @Test
  public void testSetMessageOutputLevel() {
    // Arrange
    RecorderEntry recorderEntry = new RecorderEntry(Manifest.ATTRIBUTE_NAME);

    // Act
    recorderEntry.setMessageOutputLevel(1);

    // Assert
    assertEquals(1, recorderEntry.getMessageOutputLevel());
  }

  /**
   * Test {@link RecorderEntry#setMessageOutputLevel(int)}.
   * <ul>
   *   <li>When five.</li>
   * </ul>
   * <p>
   * Method under test: {@link RecorderEntry#setMessageOutputLevel(int)}
   */
  @Test
  public void testSetMessageOutputLevel_whenFive() {
    // Arrange
    RecorderEntry recorderEntry = new RecorderEntry(Manifest.ATTRIBUTE_NAME);

    // Act
    recorderEntry.setMessageOutputLevel(5);

    // Assert that nothing has changed
    assertEquals(2, recorderEntry.getMessageOutputLevel());
  }

  /**
   * Test {@link RecorderEntry#setMessageOutputLevel(int)}.
   * <ul>
   *   <li>When minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link RecorderEntry#setMessageOutputLevel(int)}
   */
  @Test
  public void testSetMessageOutputLevel_whenMinusOne() {
    // Arrange
    RecorderEntry recorderEntry = new RecorderEntry(Manifest.ATTRIBUTE_NAME);

    // Act
    recorderEntry.setMessageOutputLevel(-1);

    // Assert that nothing has changed
    assertEquals(2, recorderEntry.getMessageOutputLevel());
  }

  /**
   * Test {@link RecorderEntry#setProject(Project)}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then {@link Project} (default constructor) BuildListeners size is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link RecorderEntry#setProject(Project)}
   */
  @Test
  public void testSetProject_givenAntClassLoader_thenProjectBuildListenersSizeIsTwo() {
    // Arrange
    RecorderEntry recorderEntry = new RecorderEntry(Manifest.ATTRIBUTE_NAME);

    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act
    recorderEntry.setProject(project);

    // Assert
    Vector<BuildListener> buildListeners = project.getBuildListeners();
    assertEquals(2, buildListeners.size());
    assertSame(recorderEntry, buildListeners.get(1));
  }

  /**
   * Test {@link RecorderEntry#setProject(Project)}.
   * <ul>
   *   <li>When {@link Project} (default constructor).</li>
   *   <li>Then {@link Project} (default constructor) BuildListeners size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link RecorderEntry#setProject(Project)}
   */
  @Test
  public void testSetProject_whenProject_thenProjectBuildListenersSizeIsOne() {
    // Arrange
    RecorderEntry recorderEntry = new RecorderEntry(Manifest.ATTRIBUTE_NAME);
    Project project = new Project();

    // Act
    recorderEntry.setProject(project);

    // Assert
    Vector<BuildListener> buildListeners = project.getBuildListeners();
    assertEquals(1, buildListeners.size());
    assertSame(recorderEntry, buildListeners.get(0));
  }

  /**
   * Test {@link RecorderEntry#cleanup()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RecorderEntry#cleanup()}
   */
  @Test
  public void testCleanup_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    RecorderEntry recorderEntry = new RecorderEntry(Manifest.ATTRIBUTE_NAME);
    recorderEntry.setProject(project);
    recorderEntry.setOutputPrintStream(null);

    // Act
    recorderEntry.cleanup();

    // Assert
    assertNull(recorderEntry.getProject());
  }

  /**
   * Test {@link RecorderEntry#cleanup()}.
   * <ul>
   *   <li>Given {@link RecorderEntry#RecorderEntry(String)} with name is {@link Manifest#ATTRIBUTE_NAME}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RecorderEntry#cleanup()}
   */
  @Test
  public void testCleanup_givenRecorderEntryWithNameIsAttribute_name() {
    // Arrange
    RecorderEntry recorderEntry = new RecorderEntry(Manifest.ATTRIBUTE_NAME);

    // Act
    recorderEntry.cleanup();

    // Assert that nothing has changed
    assertNull(recorderEntry.getProject());
  }

  /**
   * Test {@link RecorderEntry#cleanup()}.
   * <ul>
   *   <li>Given {@link RecorderEntry#RecorderEntry(String)} with name is {@link Manifest#ATTRIBUTE_NAME} Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RecorderEntry#cleanup()}
   */
  @Test
  public void testCleanup_givenRecorderEntryWithNameIsAttribute_nameProjectIsNull() {
    // Arrange
    RecorderEntry recorderEntry = new RecorderEntry(Manifest.ATTRIBUTE_NAME);
    recorderEntry.setProject(null);
    recorderEntry.setOutputPrintStream(new PrintStream(new ByteArrayOutputStream(1)));

    // Act
    recorderEntry.cleanup();

    // Assert that nothing has changed
    assertNull(recorderEntry.getProject());
  }

  /**
   * Test {@link RecorderEntry#cleanup()}.
   * <ul>
   *   <li>Given {@link RecorderEntry#RecorderEntry(String)} with name is {@link Manifest#ATTRIBUTE_NAME} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link RecorderEntry#cleanup()}
   */
  @Test
  public void testCleanup_givenRecorderEntryWithNameIsAttribute_nameProjectIsProject() {
    // Arrange
    RecorderEntry recorderEntry = new RecorderEntry(Manifest.ATTRIBUTE_NAME);
    recorderEntry.setProject(new Project());
    recorderEntry.setOutputPrintStream(null);

    // Act
    recorderEntry.cleanup();

    // Assert
    assertNull(recorderEntry.getProject());
  }
}

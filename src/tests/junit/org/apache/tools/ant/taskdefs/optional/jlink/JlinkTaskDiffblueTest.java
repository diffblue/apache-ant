package org.apache.tools.ant.taskdefs.optional.jlink;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.nio.file.Paths;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.RuntimeConfigurable;
import org.apache.tools.ant.types.Path;
import org.junit.Test;

public class JlinkTaskDiffblueTest {
  /**
   * Test {@link JlinkTask#createMergefiles()}.
   * <ul>
   *   <li>Given {@link JlinkTask} (default constructor).</li>
   *   <li>Then return Location FileName is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JlinkTask#createMergefiles()}
   */
  @Test
  public void testCreateMergefiles_givenJlinkTask_thenReturnLocationFileNameIsNull() {
    // Arrange and Act
    Path actualCreateMergefilesResult = (new JlinkTask()).createMergefiles();

    // Assert
    Location location = actualCreateMergefilesResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateMergefilesResult.getDescription());
    assertNull(actualCreateMergefilesResult.getProject());
    assertNull(actualCreateMergefilesResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualCreateMergefilesResult.size());
    assertFalse(actualCreateMergefilesResult.isReference());
    assertTrue(actualCreateMergefilesResult.isEmpty());
  }

  /**
   * Test {@link JlinkTask#setMergefiles(Path)}.
   * <ul>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JlinkTask#setMergefiles(Path)}
   */
  @Test
  public void testSetMergefiles_whenNull() {
    // Arrange
    JlinkTask jlinkTask = new JlinkTask();
    jlinkTask.setMergefiles(Path.systemBootClasspath);

    // Act
    jlinkTask.setMergefiles(null);

    // Assert that nothing has changed
    assertTrue(jlinkTask.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
  }

  /**
   * Test {@link JlinkTask#createAddfiles()}.
   * <ul>
   *   <li>Given {@link JlinkTask} (default constructor).</li>
   *   <li>Then return Location FileName is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JlinkTask#createAddfiles()}
   */
  @Test
  public void testCreateAddfiles_givenJlinkTask_thenReturnLocationFileNameIsNull() {
    // Arrange and Act
    Path actualCreateAddfilesResult = (new JlinkTask()).createAddfiles();

    // Assert
    Location location = actualCreateAddfilesResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateAddfilesResult.getDescription());
    assertNull(actualCreateAddfilesResult.getProject());
    assertNull(actualCreateAddfilesResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualCreateAddfilesResult.size());
    assertFalse(actualCreateAddfilesResult.isReference());
    assertTrue(actualCreateAddfilesResult.isEmpty());
  }

  /**
   * Test {@link JlinkTask#setAddfiles(Path)}.
   * <ul>
   *   <li>Given {@code null}.</li>
   *   <li>Then {@link Path#systemBootClasspath} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link JlinkTask#setAddfiles(Path)}
   */
  @Test
  public void testSetAddfiles_givenNull_thenSystemBootClasspathProjectIsProject() {
    // Arrange
    JlinkTask jlinkTask = new JlinkTask();
    Project project = new Project();
    jlinkTask.setAddfiles(new Path(project));
    Path addfiles = Path.systemBootClasspath;
    addfiles.setProject(null);

    // Act
    jlinkTask.setAddfiles(addfiles);

    // Assert
    assertSame(project, addfiles.getProject());
  }

  /**
   * Test {@link JlinkTask#execute()}.
   * <p>
   * Method under test: {@link JlinkTask#execute()}
   */
  @Test
  public void testExecute() throws BuildException {
    // Arrange
    JlinkTask jlinkTask = new JlinkTask();
    jlinkTask.setMergefiles(new Path(new Project(), "addfiles or mergefiles required! Please set."));
    jlinkTask.setOutfile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> jlinkTask.execute());
  }

  /**
   * Test {@link JlinkTask#execute()}.
   * <p>
   * Method under test: {@link JlinkTask#execute()}
   */
  @Test
  public void testExecute2() throws BuildException {
    // Arrange
    JlinkTask jlinkTask = new JlinkTask();
    jlinkTask.setAddfiles(new Path(new Project(), "addfiles or mergefiles required! Please set."));
    jlinkTask.setOutfile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> jlinkTask.execute());
  }

  /**
   * Test {@link JlinkTask#execute()}.
   * <ul>
   *   <li>Given {@link JlinkTask} (default constructor) Mergefiles is {@link Path#Path(Project)} with project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link JlinkTask#execute()}
   */
  @Test
  public void testExecute_givenJlinkTaskMergefilesIsPathWithProjectIsProject() throws BuildException {
    // Arrange
    JlinkTask jlinkTask = new JlinkTask();
    jlinkTask.setMergefiles(new Path(new Project()));
    jlinkTask.setOutfile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> jlinkTask.execute());
  }

  /**
   * Test {@link JlinkTask#execute()}.
   * <ul>
   *   <li>Given {@link JlinkTask} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JlinkTask#execute()}
   */
  @Test
  public void testExecute_givenJlinkTask_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new JlinkTask()).execute());
  }

  /**
   * Test {@link JlinkTask#execute()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JlinkTask#execute()}
   */
  @Test
  public void testExecute_thenThrowBuildException() throws BuildException {
    // Arrange
    JlinkTask jlinkTask = new JlinkTask();
    jlinkTask.setOutfile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> jlinkTask.execute());
  }

  /**
   * Test new {@link JlinkTask} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link JlinkTask}
   */
  @Test
  public void testNewJlinkTask() {
    // Arrange and Act
    JlinkTask actualJlinkTask = new JlinkTask();

    // Assert
    Location location = actualJlinkTask.getLocation();
    assertNull(location.getFileName());
    assertNull(actualJlinkTask.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualJlinkTask.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualJlinkTask.getTaskName());
    assertNull(actualJlinkTask.getTaskType());
    assertNull(actualJlinkTask.getProject());
    assertNull(actualJlinkTask.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualJlinkTask.hasSelectors());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualJlinkTask, runtimeConfigurableWrapper.getProxy());
  }
}

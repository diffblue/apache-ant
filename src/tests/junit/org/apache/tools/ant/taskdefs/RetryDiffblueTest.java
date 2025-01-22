package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.RuntimeConfigurable;
import org.apache.tools.ant.Task;
import org.apache.tools.ant.TaskAdapter;
import org.junit.Test;

public class RetryDiffblueTest {
  /**
   * Test {@link Retry#addTask(Task)}.
   * <ul>
   *   <li>Given {@link Retry} (default constructor) addTask {@link TaskAdapter#TaskAdapter()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Retry#addTask(Task)}
   */
  @Test
  public void testAddTask_givenRetryAddTaskTaskAdapter_thenThrowBuildException() {
    // Arrange
    Retry retry = new Retry();
    retry.addTask(new TaskAdapter());

    // Act and Assert
    assertThrows(BuildException.class, () -> retry.addTask(new TaskAdapter()));
  }

  /**
   * Test {@link Retry#setRetryDelay(int)}.
   * <ul>
   *   <li>When minus one.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Retry#setRetryDelay(int)}
   */
  @Test
  public void testSetRetryDelay_whenMinusOne_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Retry()).setRetryDelay(-1));
  }

  /**
   * Test {@link Retry#execute()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Retry#execute()}
   */
  @Test
  public void testExecute_givenJavaLangObject_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("]: error occurred; retrying...", typeClass);
    project.addBuildListener(new AntClassLoader());

    Retry retry = new Retry();
    retry.setProject(project);
    retry.addTask(new TaskAdapter());

    // Act and Assert
    assertThrows(BuildException.class, () -> retry.execute());
  }

  /**
   * Test {@link Retry#execute()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Retry#execute()}
   */
  @Test
  public void testExecute_givenProjectAddBuildListenerAntClassLoader_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Retry retry = new Retry();
    retry.setProject(project);
    retry.addTask(new TaskAdapter());

    // Act and Assert
    assertThrows(BuildException.class, () -> retry.execute());
  }

  /**
   * Test {@link Retry#execute()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link RecorderEntry#RecorderEntry(String)} with name is {@code Attempt [}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Retry#execute()}
   */
  @Test
  public void testExecute_givenProjectAddBuildListenerRecorderEntryWithNameIsAttempt() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new RecorderEntry("Attempt ["));

    Retry retry = new Retry();
    retry.setProject(project);
    retry.addTask(new TaskAdapter());

    // Act and Assert
    assertThrows(BuildException.class, () -> retry.execute());
  }

  /**
   * Test {@link Retry#execute()}.
   * <ul>
   *   <li>Given {@link Retry} (default constructor) addTask {@link TaskAdapter#TaskAdapter()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Retry#execute()}
   */
  @Test
  public void testExecute_givenRetryAddTaskTaskAdapter_thenThrowBuildException() throws BuildException {
    // Arrange
    Retry retry = new Retry();
    retry.addTask(new TaskAdapter());

    // Act and Assert
    assertThrows(BuildException.class, () -> retry.execute());
  }

  /**
   * Test {@link Retry#execute()}.
   * <ul>
   *   <li>Given {@link Retry} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Retry#execute()}
   */
  @Test
  public void testExecute_givenRetryProjectIsProject_thenThrowBuildException() throws BuildException {
    // Arrange
    Retry retry = new Retry();
    retry.setProject(new Project());
    retry.addTask(new TaskAdapter());

    // Act and Assert
    assertThrows(BuildException.class, () -> retry.execute());
  }

  /**
   * Test {@link Retry#execute()}.
   * <ul>
   *   <li>Given {@link TaskAdapter#TaskAdapter()} Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Retry#execute()}
   */
  @Test
  public void testExecute_givenTaskAdapterProjectIsProject_thenThrowBuildException() throws BuildException {
    // Arrange
    TaskAdapter t = new TaskAdapter();
    t.setProject(new Project());

    Retry retry = new Retry();
    retry.addTask(t);

    // Act and Assert
    assertThrows(BuildException.class, () -> retry.execute());
  }

  /**
   * Test {@link Retry#execute()}.
   * <ul>
   *   <li>Given {@link TaskAdapter#TaskAdapter()} Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Retry#execute()}
   */
  @Test
  public void testExecute_givenTaskAdapterProjectIsProject_thenThrowBuildException2() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    TaskAdapter t = new TaskAdapter();
    t.setProject(project);

    Retry retry = new Retry();
    retry.addTask(t);

    // Act and Assert
    assertThrows(BuildException.class, () -> retry.execute());
  }

  /**
   * Test new {@link Retry} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Retry}
   */
  @Test
  public void testNewRetry() {
    // Arrange and Act
    Retry actualRetry = new Retry();

    // Assert
    Location location = actualRetry.getLocation();
    assertNull(location.getFileName());
    assertNull(actualRetry.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualRetry.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualRetry.getTaskName());
    assertNull(actualRetry.getTaskType());
    assertNull(actualRetry.getProject());
    assertNull(actualRetry.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualRetry, runtimeConfigurableWrapper.getProxy());
  }
}

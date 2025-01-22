package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.RuntimeConfigurable;
import org.apache.tools.ant.TaskAdapter;
import org.apache.tools.ant.taskdefs.Parallel.TaskList;
import org.junit.Test;

public class ParallelDiffblueTest {
  /**
   * Test {@link Parallel#addDaemons(TaskList)}.
   * <ul>
   *   <li>Given {@link Parallel} (default constructor) addDaemons {@link TaskList} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Parallel#addDaemons(TaskList)}
   */
  @Test
  public void testAddDaemons_givenParallelAddDaemonsTaskList_thenThrowBuildException() {
    // Arrange
    Parallel parallel = new Parallel();
    parallel.addDaemons(new TaskList());

    // Act and Assert
    assertThrows(BuildException.class, () -> parallel.addDaemons(new TaskList()));
  }

  /**
   * Test {@link Parallel#execute()}.
   * <ul>
   *   <li>Given {@link Parallel} (default constructor) addTask {@link TaskAdapter#TaskAdapter()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Parallel#execute()}
   */
  @Test
  public void testExecute_givenParallelAddTaskTaskAdapter_thenThrowBuildException() throws BuildException {
    // Arrange
    Parallel parallel = new Parallel();
    parallel.addTask(new TaskAdapter());
    parallel.addTask(new TaskAdapter());
    parallel.addDaemons(new TaskList());

    // Act and Assert
    assertThrows(BuildException.class, () -> parallel.execute());
  }

  /**
   * Test {@link Parallel#execute()}.
   * <ul>
   *   <li>Given {@link Parallel} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Parallel#execute()}
   */
  @Test
  public void testExecute_givenParallelProjectIsProject_thenThrowBuildException() throws BuildException {
    // Arrange
    Parallel parallel = new Parallel();
    parallel.setProject(new Project());
    parallel.addTask(new TaskAdapter());
    parallel.addTask(new TaskAdapter());
    parallel.addDaemons(new TaskList());

    // Act and Assert
    assertThrows(BuildException.class, () -> parallel.execute());
  }

  /**
   * Test new {@link Parallel} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Parallel}
   */
  @Test
  public void testNewParallel() {
    // Arrange and Act
    Parallel actualParallel = new Parallel();

    // Assert
    Location location = actualParallel.getLocation();
    assertNull(location.getFileName());
    assertNull(actualParallel.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualParallel.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualParallel.getTaskName());
    assertNull(actualParallel.getTaskType());
    assertNull(actualParallel.getProject());
    assertNull(actualParallel.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualParallel, runtimeConfigurableWrapper.getProxy());
  }
}

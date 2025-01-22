package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.PickOneTask;
import org.apache.tools.ant.Task;
import org.apache.tools.ant.TaskAdapter;
import org.apache.tools.ant.UnknownElement;
import org.apache.tools.ant.taskdefs.Ant;
import org.apache.tools.ant.taskdefs.AntStructure;
import org.apache.tools.ant.taskdefs.Antlib;
import org.apache.tools.ant.taskdefs.AntlibDefinition;
import org.junit.Test;

public class WorkerAntDiffblueTest {
  /**
   * Test {@link WorkerAnt#WorkerAnt(Task)}.
   * <p>
   * Method under test: {@link WorkerAnt#WorkerAnt(Task)}
   */
  @Test
  public void testNewWorkerAnt() {
    // Arrange
    TaskAdapter task = new TaskAdapter();

    // Act
    WorkerAnt actualWorkerAnt = new WorkerAnt(task);

    // Assert
    assertNull(actualWorkerAnt.getException());
    assertNull(actualWorkerAnt.getBuildException());
    assertFalse(actualWorkerAnt.isFinished());
    assertSame(task, actualWorkerAnt.getTask());
  }

  /**
   * Test {@link WorkerAnt#WorkerAnt(Task, Object)}.
   * <ul>
   *   <li>When {@code Notify}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WorkerAnt#WorkerAnt(Task, Object)}
   */
  @Test
  public void testNewWorkerAnt_whenNotify() {
    // Arrange
    TaskAdapter task = new TaskAdapter();

    // Act
    WorkerAnt actualWorkerAnt = new WorkerAnt(task, "Notify");

    // Assert
    assertNull(actualWorkerAnt.getException());
    assertNull(actualWorkerAnt.getBuildException());
    assertFalse(actualWorkerAnt.isFinished());
    assertSame(task, actualWorkerAnt.getTask());
  }

  /**
   * Test {@link WorkerAnt#WorkerAnt(Task, Object)}.
   * <ul>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WorkerAnt#WorkerAnt(Task, Object)}
   */
  @Test
  public void testNewWorkerAnt_whenNull() {
    // Arrange
    TaskAdapter task = new TaskAdapter();

    // Act
    WorkerAnt actualWorkerAnt = new WorkerAnt(task, null);

    // Assert
    assertNull(actualWorkerAnt.getException());
    assertNull(actualWorkerAnt.getBuildException());
    assertFalse(actualWorkerAnt.isFinished());
    assertSame(task, actualWorkerAnt.getTask());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link WorkerAnt#getBuildException()}
   *   <li>{@link WorkerAnt#getException()}
   *   <li>{@link WorkerAnt#getTask()}
   *   <li>{@link WorkerAnt#isFinished()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    WorkerAnt workerAnt = new WorkerAnt(task);

    // Act
    BuildException actualBuildException = workerAnt.getBuildException();
    Throwable actualException = workerAnt.getException();
    Task actualTask = workerAnt.getTask();

    // Assert
    assertNull(actualException);
    assertNull(actualBuildException);
    assertFalse(workerAnt.isFinished());
    assertSame(task, actualTask);
  }

  /**
   * Test {@link WorkerAnt#run()}.
   * <p>
   * Method under test: {@link WorkerAnt#run()}
   */
  @Test
  public void testRun() {
    // Arrange
    WorkerAnt workerAnt = new WorkerAnt(new TaskAdapter());

    // Act
    workerAnt.run();

    // Assert
    Throwable exception = workerAnt.getException();
    assertEquals("Cannot invoke \"Object.getClass()\" because \"this.proxy\" is null", exception.getLocalizedMessage());
    assertEquals("Cannot invoke \"Object.getClass()\" because \"this.proxy\" is null", exception.getMessage());
    BuildException buildException = workerAnt.getBuildException();
    assertEquals("java.lang.NullPointerException: Cannot invoke \"Object.getClass()\" because \"this.proxy\" is null",
        buildException.getLocalizedMessage());
    assertEquals("java.lang.NullPointerException: Cannot invoke \"Object.getClass()\" because \"this.proxy\" is null",
        buildException.getMessage());
    assertSame(exception, buildException.getCause());
    assertSame(exception, buildException.getException());
  }

  /**
   * Test {@link WorkerAnt#run()}.
   * <p>
   * Method under test: {@link WorkerAnt#run()}
   */
  @Test
  public void testRun2() {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    task.setProxy("42");
    WorkerAnt workerAnt = new WorkerAnt(task);

    // Act
    workerAnt.run();

    // Assert
    BuildException buildException = workerAnt.getBuildException();
    assertEquals("java.lang.NoSuchMethodException: java.lang.String.execute()", buildException.getLocalizedMessage());
    assertEquals("java.lang.NoSuchMethodException: java.lang.String.execute()", buildException.getMessage());
    Throwable cause = buildException.getCause();
    assertEquals("java.lang.String.execute()", cause.getLocalizedMessage());
    assertEquals("java.lang.String.execute()", cause.getMessage());
  }

  /**
   * Test {@link WorkerAnt#run()}.
   * <ul>
   *   <li>Given {@link WorkerAnt#WorkerAnt(Task)} with task is {@code null}.</li>
   *   <li>Then {@link WorkerAnt#WorkerAnt(Task)} with task is {@code null} Exception is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WorkerAnt#run()}
   */
  @Test
  public void testRun_givenWorkerAntWithTaskIsNull_thenWorkerAntWithTaskIsNullExceptionIsNull() {
    // Arrange
    WorkerAnt workerAnt = new WorkerAnt(null);

    // Act
    workerAnt.run();

    // Assert
    assertNull(workerAnt.getException());
    assertTrue(workerAnt.isFinished());
  }

  /**
   * Test {@link WorkerAnt#run()}.
   * <ul>
   *   <li>Then {@link WorkerAnt#WorkerAnt(Task)} with task is {@link Ant#Ant()} Exception LocalizedMessage is a string.</li>
   * </ul>
   * <p>
   * Method under test: {@link WorkerAnt#run()}
   */
  @Test
  public void testRun_thenWorkerAntWithTaskIsAntExceptionLocalizedMessageIsAString() {
    // Arrange
    WorkerAnt workerAnt = new WorkerAnt(new Ant());

    // Act
    workerAnt.run();

    // Assert
    Throwable exception = workerAnt.getException();
    assertEquals("Cannot invoke \"org.apache.tools.ant.Project.createSubProject()\" because the return value of"
        + " \"org.apache.tools.ant.taskdefs.Ant.getProject()\" is null", exception.getLocalizedMessage());
    assertEquals("Cannot invoke \"org.apache.tools.ant.Project.createSubProject()\" because the return value of"
        + " \"org.apache.tools.ant.taskdefs.Ant.getProject()\" is null", exception.getMessage());
    BuildException buildException = workerAnt.getBuildException();
    assertEquals(
        "java.lang.NullPointerException: Cannot invoke \"org.apache.tools.ant.Project.createSubProject()\" because"
            + " the return value of \"org.apache.tools.ant.taskdefs.Ant.getProject()\" is null",
        buildException.getLocalizedMessage());
    assertEquals(
        "java.lang.NullPointerException: Cannot invoke \"org.apache.tools.ant.Project.createSubProject()\" because"
            + " the return value of \"org.apache.tools.ant.taskdefs.Ant.getProject()\" is null",
        buildException.getMessage());
    assertSame(exception, buildException.getCause());
    assertSame(exception, buildException.getException());
  }

  /**
   * Test {@link WorkerAnt#run()}.
   * <ul>
   *   <li>Then {@link WorkerAnt#WorkerAnt(Task)} with task is {@link AntStructure} (default constructor) Task {@link AntStructure}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WorkerAnt#run()}
   */
  @Test
  public void testRun_thenWorkerAntWithTaskIsAntStructureTaskAntStructure() {
    // Arrange
    WorkerAnt workerAnt = new WorkerAnt(new AntStructure());

    // Act
    workerAnt.run();

    // Assert
    assertTrue(workerAnt.getTask() instanceof AntStructure);
    BuildException buildException = workerAnt.getBuildException();
    assertEquals("output attribute is required", buildException.getLocalizedMessage());
    assertEquals("output attribute is required", buildException.getMessage());
    assertSame(buildException, workerAnt.getException());
  }

  /**
   * Test {@link WorkerAnt#run()}.
   * <ul>
   *   <li>Then {@link WorkerAnt#WorkerAnt(Task)} with task is {@link AntlibDefinition} (default constructor) Exception is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WorkerAnt#run()}
   */
  @Test
  public void testRun_thenWorkerAntWithTaskIsAntlibDefinitionExceptionIsNull() {
    // Arrange
    WorkerAnt workerAnt = new WorkerAnt(new AntlibDefinition());

    // Act
    workerAnt.run();

    // Assert
    assertNull(workerAnt.getException());
    assertTrue(workerAnt.isFinished());
  }

  /**
   * Test {@link WorkerAnt#run()}.
   * <ul>
   *   <li>Then {@link WorkerAnt#WorkerAnt(Task)} with task is {@link Antlib} (default constructor) Exception is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WorkerAnt#run()}
   */
  @Test
  public void testRun_thenWorkerAntWithTaskIsAntlibExceptionIsNull() {
    // Arrange
    WorkerAnt workerAnt = new WorkerAnt(new Antlib());

    // Act
    workerAnt.run();

    // Assert
    assertNull(workerAnt.getException());
    assertTrue(workerAnt.isFinished());
  }

  /**
   * Test {@link WorkerAnt#run()}.
   * <ul>
   *   <li>Then {@link WorkerAnt#WorkerAnt(Task)} with task is {@link TaskAdapter#TaskAdapter()} Task Proxy {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WorkerAnt#run()}
   */
  @Test
  public void testRun_thenWorkerAntWithTaskIsTaskAdapterTaskProxyBuildException() {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    task.setProxy(new BuildException());
    WorkerAnt workerAnt = new WorkerAnt(task);

    // Act
    workerAnt.run();

    // Assert
    Task task2 = workerAnt.getTask();
    assertTrue(((TaskAdapter) task2).getProxy() instanceof BuildException);
    assertTrue(task2 instanceof TaskAdapter);
    BuildException buildException = workerAnt.getBuildException();
    assertEquals("java.lang.NoSuchMethodException: org.apache.tools.ant.BuildException.execute()",
        buildException.getLocalizedMessage());
    assertEquals("java.lang.NoSuchMethodException: org.apache.tools.ant.BuildException.execute()",
        buildException.getMessage());
    Throwable cause = buildException.getCause();
    assertEquals("org.apache.tools.ant.BuildException.execute()", cause.getLocalizedMessage());
    assertEquals("org.apache.tools.ant.BuildException.execute()", cause.getMessage());
  }

  /**
   * Test {@link WorkerAnt#run()}.
   * <ul>
   *   <li>Then {@link WorkerAnt#WorkerAnt(Task)} with task is {@link TaskAdapter#TaskAdapter()} Task Proxy {@link PickOneTask}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WorkerAnt#run()}
   */
  @Test
  public void testRun_thenWorkerAntWithTaskIsTaskAdapterTaskProxyPickOneTask() {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    task.setProxy(new PickOneTask());
    WorkerAnt workerAnt = new WorkerAnt(task);

    // Act
    workerAnt.run();

    // Assert
    Task task2 = workerAnt.getTask();
    assertTrue(((TaskAdapter) task2).getProxy() instanceof PickOneTask);
    assertTrue(task2 instanceof TaskAdapter);
    BuildException buildException = workerAnt.getBuildException();
    assertEquals("Dispatchable Task attribute 'action' not set or value is empty.",
        buildException.getLocalizedMessage());
    assertEquals("Dispatchable Task attribute 'action' not set or value is empty.", buildException.getMessage());
    assertNull(buildException.getCause());
    assertNull(buildException.getException());
  }

  /**
   * Test {@link WorkerAnt#run()}.
   * <ul>
   *   <li>Then {@link WorkerAnt#WorkerAnt(Task)} with task is {@link TaskAdapter#TaskAdapter()} Task Proxy {@link UnknownElement}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WorkerAnt#run()}
   */
  @Test
  public void testRun_thenWorkerAntWithTaskIsTaskAdapterTaskProxyUnknownElement() {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    task.setProxy(new UnknownElement("setLocation"));
    WorkerAnt workerAnt = new WorkerAnt(task);

    // Act
    workerAnt.run();

    // Assert
    Task task2 = workerAnt.getTask();
    assertTrue(task2 instanceof TaskAdapter);
    assertTrue(((TaskAdapter) task2).getProxy() instanceof UnknownElement);
    assertNull(workerAnt.getException());
    assertTrue(workerAnt.isFinished());
  }
}

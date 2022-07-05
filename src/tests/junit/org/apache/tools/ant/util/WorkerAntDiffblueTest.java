package org.apache.tools.ant.util;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.nio.file.Paths;
import org.apache.ant.antunit.AntUnit;
import org.apache.ant.antunit.AssertionFailedException;
import org.apache.ant.antunit.ExpectFailureTask;
import org.apache.ant.antunit.LogContent;
import org.apache.ivy.ant.AddPathTask;
import org.apache.ivy.ant.BuildOBRTask;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Task;
import org.apache.tools.ant.UnknownElement;
import org.apache.tools.ant.XmlLogger;
import org.apache.tools.ant.types.DirSet;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.ResourceCollection;
import org.junit.Test;

public class WorkerAntDiffblueTest {
  /**
  * Methods under test: 
  * 
  * <ul>
  *   <li>{@link WorkerAnt#WorkerAnt(Task)}
  *   <li>{@link WorkerAnt#getBuildException()}
  *   <li>{@link WorkerAnt#getException()}
  *   <li>{@link WorkerAnt#getTask()}
  *   <li>{@link WorkerAnt#isFinished()}
  * </ul>
  */
  @Test
  public void testConstructor() {
    // Arrange
    AntUnit antUnit = new AntUnit();

    // Act
    WorkerAnt actualWorkerAnt = new WorkerAnt(antUnit);

    // Assert
    assertNull(actualWorkerAnt.getBuildException());
    assertNull(actualWorkerAnt.getException());
    assertSame(antUnit, actualWorkerAnt.getTask());
    assertFalse(actualWorkerAnt.isFinished());
  }

  /**
   * Method under test: {@link WorkerAnt#WorkerAnt(Task)}
   */
  @Test
  public void testConstructor2() {
    // Arrange
    AntUnit antUnit = new AntUnit();

    // Act
    WorkerAnt actualWorkerAnt = new WorkerAnt(antUnit);

    // Assert
    assertFalse(actualWorkerAnt.isFinished());
    assertSame(antUnit, actualWorkerAnt.getTask());
  }

  /**
   * Method under test: {@link WorkerAnt#WorkerAnt(Task, Object)}
   */
  @Test
  public void testConstructor3() {
    // Arrange
    AntUnit antUnit = new AntUnit();

    // Act
    WorkerAnt actualWorkerAnt = new WorkerAnt(antUnit, "Notify");

    // Assert
    assertFalse(actualWorkerAnt.isFinished());
    assertSame(antUnit, actualWorkerAnt.getTask());
  }

  /**
   * Method under test: {@link WorkerAnt#WorkerAnt(Task, Object)}
   */
  @Test
  public void testConstructor4() {
    // Arrange
    AntUnit antUnit = new AntUnit();

    // Act
    WorkerAnt actualWorkerAnt = new WorkerAnt(antUnit, null);

    // Assert
    assertFalse(actualWorkerAnt.isFinished());
    assertSame(antUnit, actualWorkerAnt.getTask());
  }

  /**
   * Method under test: {@link WorkerAnt#rethrowAnyBuildException()}
   */
  @Test
  public void testRethrowAnyBuildException() {
    // Arrange
    AntUnit antUnit = new AntUnit();
    WorkerAnt workerAnt = new WorkerAnt(antUnit);

    // Act
    workerAnt.rethrowAnyBuildException();

    // Assert that nothing has changed
    assertFalse(workerAnt.isFinished());
    assertSame(antUnit, workerAnt.getTask());
  }

  /**
   * Method under test: {@link WorkerAnt#run()}
   */
  @Test
  public void testRun() {
    // Arrange
    WorkerAnt workerAnt = new WorkerAnt(new AntUnit());

    // Act
    workerAnt.run();

    // Assert
    BuildException buildException = workerAnt.getBuildException();
    assertSame(buildException, workerAnt.getException());
    assertTrue(workerAnt.isFinished());
    Location expectedLocation = workerAnt.getTask().getLocation();
    assertSame(expectedLocation, buildException.getLocation());
  }

  /**
   * Method under test: {@link WorkerAnt#run()}
   */
  @Test
  public void testRun2() {
    // Arrange
    WorkerAnt workerAnt = new WorkerAnt(null);

    // Act
    workerAnt.run();

    // Assert
    assertTrue(workerAnt.isFinished());
  }

  /**
   * Method under test: {@link WorkerAnt#run()}
   */
  @Test
  public void testRun3() {
    // Arrange
    WorkerAnt workerAnt = new WorkerAnt(new ExpectFailureTask());

    // Act
    workerAnt.run();

    // Assert
    assertTrue(workerAnt.isFinished());
  }

  /**
   * Method under test: {@link WorkerAnt#run()}
   */
  @Test
  public void testRun4() {
    // Arrange
    AntUnit antUnit = new AntUnit();
    antUnit.add(new LogContent());
    WorkerAnt workerAnt = new WorkerAnt(antUnit);

    // Act
    workerAnt.run();

    // Assert
    BuildException buildException = workerAnt.getBuildException();
    assertSame(buildException, workerAnt.getException());
    assertTrue(workerAnt.isFinished());
    Location expectedLocation = workerAnt.getTask().getLocation();
    assertSame(expectedLocation, buildException.getLocation());
  }

  /**
   * Method under test: {@link WorkerAnt#run()}
   */
  @Test
  public void testRun5() {
    // Arrange
    AntUnit antUnit = new AntUnit();
    antUnit.add(new DirSet());
    WorkerAnt workerAnt = new WorkerAnt(antUnit);

    // Act
    workerAnt.run();

    // Assert
    BuildException buildException = workerAnt.getBuildException();
    assertSame(buildException, workerAnt.getException());
    assertTrue(workerAnt.isFinished());
    Location expectedLocation = workerAnt.getTask().getLocation();
    assertSame(expectedLocation, buildException.getLocation());
  }

  /**
   * Method under test: {@link WorkerAnt#run()}
   */
  @Test
  public void testRun6() {
    // Arrange
    AntUnit antUnit = new AntUnit();
    antUnit.add((ResourceCollection) null);
    WorkerAnt workerAnt = new WorkerAnt(antUnit);

    // Act
    workerAnt.run();

    // Assert
    assertTrue(workerAnt.isFinished());
  }

  /**
   * Method under test: {@link WorkerAnt#run()}
   */
  @Test
  public void testRun7() {
    // Arrange
    AntUnit antUnit = new AntUnit();
    antUnit.add(new Path(new Project(), "Only file system resources are supported."));
    WorkerAnt workerAnt = new WorkerAnt(antUnit);

    // Act
    workerAnt.run();

    // Assert
    assertTrue(workerAnt.isFinished());
  }

  /**
   * Method under test: {@link WorkerAnt#run()}
   */
  @Test
  public void testRun8() {
    // Arrange
    ExpectFailureTask expectFailureTask = new ExpectFailureTask();
    expectFailureTask.setProject(new Project());
    WorkerAnt workerAnt = new WorkerAnt(expectFailureTask);

    // Act
    workerAnt.run();

    // Assert
    BuildException buildException = workerAnt.getBuildException();
    assertTrue(buildException instanceof AssertionFailedException);
    assertTrue(workerAnt.isFinished());
    assertSame(buildException, workerAnt.getException());
  }

  /**
   * Method under test: {@link WorkerAnt#run()}
   */
  @Test
  public void testRun9() {
    // Arrange
    AntUnit antUnit = new AntUnit();
    antUnit.add(new Path(new Project(), "."));
    WorkerAnt workerAnt = new WorkerAnt(antUnit);

    // Act
    workerAnt.run();

    // Assert
    assertTrue(workerAnt.isFinished());
  }

  /**
   * Method under test: {@link WorkerAnt#run()}
   */
  @Test
  public void testRun10() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    ExpectFailureTask expectFailureTask = new ExpectFailureTask();
    expectFailureTask.setProject(project);
    WorkerAnt workerAnt = new WorkerAnt(expectFailureTask);

    // Act
    workerAnt.run();

    // Assert
    BuildException buildException = workerAnt.getBuildException();
    assertTrue(buildException instanceof AssertionFailedException);
    assertTrue(workerAnt.isFinished());
    assertSame(buildException, workerAnt.getException());
  }

  /**
   * Method under test: {@link WorkerAnt#run()}
   */
  @Test
  public void testRun11() {
    // Arrange
    DirSet dirSet = new DirSet();
    dirSet.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    AntUnit antUnit = new AntUnit();
    antUnit.add(dirSet);
    WorkerAnt workerAnt = new WorkerAnt(antUnit);

    // Act
    workerAnt.run();

    // Assert
    assertTrue(workerAnt.isFinished());
  }

  /**
   * Method under test: {@link WorkerAnt#run()}
   */
  @Test
  public void testRun12() {
    // Arrange
    AntUnit antUnit = new AntUnit();
    antUnit.setProject(new Project());
    antUnit.add(new Path(new Project(), "Only file system resources are supported."));
    WorkerAnt workerAnt = new WorkerAnt(antUnit);

    // Act
    workerAnt.run();

    // Assert
    assertTrue(workerAnt.isFinished());
  }

  /**
   * Method under test: {@link WorkerAnt#run()}
   */
  @Test
  public void testRun13() {
    // Arrange
    AntUnit antUnit = new AntUnit();
    antUnit.setProject(new Project());
    antUnit.add(new Path(new Project(), "."));
    WorkerAnt workerAnt = new WorkerAnt(antUnit);

    // Act
    workerAnt.run();

    // Assert
    BuildException buildException = workerAnt.getBuildException();
    assertSame(buildException, workerAnt.getException());
    assertTrue(workerAnt.isFinished());
    Location expectedLocation = workerAnt.getTask().getLocation();
    assertSame(expectedLocation, buildException.getLocation());
  }

  /**
   * Method under test: {@link WorkerAnt#run()}
   */
  @Test
  public void testRun14() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    UnknownElement unknownElement = new UnknownElement("ant.LocalProperties");
    unknownElement.setProject(project);
    WorkerAnt workerAnt = new WorkerAnt(unknownElement);

    // Act
    workerAnt.run();

    // Assert
    assertTrue(workerAnt.isFinished());
  }

  /**
   * Method under test: {@link WorkerAnt#run()}
   */
  @Test
  public void testRun15() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    AddPathTask addPathTask = new AddPathTask();
    addPathTask.setProject(project);
    WorkerAnt workerAnt = new WorkerAnt(addPathTask);

    // Act
    workerAnt.run();

    // Assert
    assertTrue(workerAnt.isFinished());
  }

  /**
   * Method under test: {@link WorkerAnt#run()}
   */
  @Test
  public void testRun16() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    ExpectFailureTask expectFailureTask = new ExpectFailureTask();
    expectFailureTask.addTask(new AntUnit());
    expectFailureTask.setProject(project);
    WorkerAnt workerAnt = new WorkerAnt(expectFailureTask);

    // Act
    workerAnt.run();

    // Assert
    assertTrue(workerAnt.isFinished());
  }

  /**
   * Method under test: {@link WorkerAnt#run()}
   */
  @Test
  public void testRun17() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    BuildOBRTask buildOBRTask = new BuildOBRTask();
    buildOBRTask.setProject(project);
    WorkerAnt workerAnt = new WorkerAnt(buildOBRTask);

    // Act
    workerAnt.run();

    // Assert
    BuildException buildException = workerAnt.getBuildException();
    assertSame(buildException, workerAnt.getException());
    assertTrue(workerAnt.isFinished());
    Location expectedLocation = workerAnt.getTask().getLocation();
    assertSame(expectedLocation, buildException.getLocation());
  }

  /**
   * Method under test: {@link WorkerAnt#run()}
   */
  @Test
  public void testRun18() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(null);

    ExpectFailureTask expectFailureTask = new ExpectFailureTask();
    expectFailureTask.setProject(project);
    WorkerAnt workerAnt = new WorkerAnt(expectFailureTask);

    // Act
    workerAnt.run();

    // Assert
    assertTrue(workerAnt.isFinished());
  }

  /**
   * Method under test: {@link WorkerAnt#run()}
   */
  @Test
  public void testRun19() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new XmlLogger());

    ExpectFailureTask expectFailureTask = new ExpectFailureTask();
    expectFailureTask.setProject(project);
    WorkerAnt workerAnt = new WorkerAnt(expectFailureTask);

    // Act
    workerAnt.run();

    // Assert
    assertTrue(workerAnt.isFinished());
  }
}


package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.io.IOException;
import java.util.Hashtable;
import org.apache.ant.antunit.AntUnit;
import org.apache.ant.antunit.LogContent;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.ProjectComponent;
import org.apache.tools.ant.Target;
import org.apache.tools.ant.taskdefs.Concat;
import org.apache.tools.ant.types.FileList;
import org.apache.tools.ant.types.LogLevel;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.ResourceCollection;
import org.apache.tools.ant.util.facade.ImplementationSpecificArgument;
import org.junit.Test;

public class ConcatResourceInputStreamDiffblueTest {
  /**
  * Methods under test: 
  * 
  * <ul>
  *   <li>{@link ConcatResourceInputStream#ConcatResourceInputStream(ResourceCollection)}
  *   <li>{@link ConcatResourceInputStream#setIgnoreErrors(boolean)}
  *   <li>{@link ConcatResourceInputStream#setManagingComponent(ProjectComponent)}
  *   <li>{@link ConcatResourceInputStream#isIgnoreErrors()}
  * </ul>
  */
  @Test
  public void testConstructor() {
    // Arrange and Act
    ConcatResourceInputStream actualConcatResourceInputStream = new ConcatResourceInputStream(new LogContent());
    actualConcatResourceInputStream.setIgnoreErrors(true);
    actualConcatResourceInputStream.setManagingComponent(new AntUnit());

    // Assert
    assertTrue(actualConcatResourceInputStream.isIgnoreErrors());
  }

  /**
   * Method under test: {@link ConcatResourceInputStream#ConcatResourceInputStream(ResourceCollection)}
   */
  @Test
  public void testConstructor2() {
    // Arrange, Act and Assert
    assertFalse((new ConcatResourceInputStream(new LogContent())).isIgnoreErrors());
  }

  /**
   * Method under test: {@link ConcatResourceInputStream#ConcatResourceInputStream(ResourceCollection)}
   */
  @Test
  public void testConstructor3() {
    // Arrange
    Path path = new Path(new Project(), "Path");

    // Act and Assert
    assertFalse((new ConcatResourceInputStream(path)).isIgnoreErrors());
    assertEquals(1, path.size());
  }

  /**
   * Method under test: {@link ConcatResourceInputStream#ConcatResourceInputStream(ResourceCollection)}
   */
  @Test
  public void testConstructor4() {
    // Arrange
    Concat concat = new Concat();
    concat.addFilelist(new FileList());

    // Act and Assert
    assertFalse((new ConcatResourceInputStream(concat)).isIgnoreErrors());
  }

  /**
   * Method under test: {@link ConcatResourceInputStream#ConcatResourceInputStream(ResourceCollection)}
   */
  @Test
  public void testConstructor5() {
    // Arrange
    Concat concat = new Concat();
    concat.addText("At least one resource must be provided, or some text.");

    // Act and Assert
    assertFalse((new ConcatResourceInputStream(concat)).isIgnoreErrors());
  }

  /**
   * Method under test: {@link ConcatResourceInputStream#ConcatResourceInputStream(ResourceCollection)}
   */
  @Test
  public void testConstructor6() {
    // Arrange
    Concat concat = new Concat();
    concat.setProject(new Project());
    concat.addText("At least one resource must be provided, or some text.");

    // Act and Assert
    assertFalse((new ConcatResourceInputStream(concat)).isIgnoreErrors());
    Project project = concat.getProject();
    Hashtable<String, Object> userProperties = project.getUserProperties();
    assertTrue(userProperties.isEmpty());
    assertEquals(userProperties, project.getTaskDefinitions());
    Hashtable<String, Target> expectedInheritedProperties = project.getTargets();
    assertEquals(expectedInheritedProperties, project.getInheritedProperties());
    assertNull(project.getDescription());
  }

  /**
   * Method under test: {@link ConcatResourceInputStream#log(String, int)}
   */
  @Test
  public void testLog() {
    // Arrange
    ConcatResourceInputStream concatResourceInputStream = new ConcatResourceInputStream(new LogContent());

    // Act
    concatResourceInputStream.log("Not all who wander are lost", 1);

    // Assert that nothing has changed
    assertFalse(concatResourceInputStream.isIgnoreErrors());
  }

  /**
   * Method under test: {@link ConcatResourceInputStream#log(String, int)}
   */
  @Test
  public void testLog2() {
    // Arrange
    ConcatResourceInputStream concatResourceInputStream = new ConcatResourceInputStream(new LogContent());
    concatResourceInputStream.setManagingComponent(new AntUnit());

    // Act
    concatResourceInputStream.log("Not all who wander are lost", 1);

    // Assert that nothing has changed
    assertFalse(concatResourceInputStream.isIgnoreErrors());
  }

  /**
   * Method under test: {@link ConcatResourceInputStream#log(String, int)}
   */
  @Test
  public void testLog3() {
    // Arrange
    ConcatResourceInputStream concatResourceInputStream = new ConcatResourceInputStream(new LogContent());

    // Act
    concatResourceInputStream.log("Not all who wander are lost", 3);

    // Assert that nothing has changed
    assertFalse(concatResourceInputStream.isIgnoreErrors());
  }

  /**
   * Method under test: {@link ConcatResourceInputStream#log(String, int)}
   */
  @Test
  public void testLog4() {
    // Arrange
    ConcatResourceInputStream concatResourceInputStream = new ConcatResourceInputStream(new LogContent());
    concatResourceInputStream.setManagingComponent(new ImplementationSpecificArgument());

    // Act
    concatResourceInputStream.log("Not all who wander are lost", 1);

    // Assert that nothing has changed
    assertFalse(concatResourceInputStream.isIgnoreErrors());
  }

  /**
   * Method under test: {@link ConcatResourceInputStream#log(String, int)}
   */
  @Test
  public void testLog5() {
    // Arrange
    ConcatResourceInputStream concatResourceInputStream = new ConcatResourceInputStream(new LogContent());
    concatResourceInputStream.setManagingComponent(new AntUnit());

    // Act
    concatResourceInputStream.log("Not all who wander are lost", 3);

    // Assert that nothing has changed
    assertFalse(concatResourceInputStream.isIgnoreErrors());
  }

  /**
   * Method under test: {@link ConcatResourceInputStream#read()}
   */
  @Test
  public void testRead() throws IOException {
    // Arrange, Act and Assert
    assertEquals(Retryable.RETRY_FOREVER, (new ConcatResourceInputStream(new Resource("ant.antunit.log"))).read());
  }

  /**
   * Method under test: {@link ConcatResourceInputStream#read()}
   */
  @Test
  public void testRead2() throws IOException {
    // Arrange
    Project p = new Project();

    // Act and Assert
    assertEquals(Retryable.RETRY_FOREVER, (new ConcatResourceInputStream(new LogContent(p, new LogLevel()))).read());
  }

  /**
   * Method under test: {@link ConcatResourceInputStream#read()}
   */
  @Test
  public void testRead3() throws IOException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertEquals(Retryable.RETRY_FOREVER,
        (new ConcatResourceInputStream(new LogContent(project, new LogLevel()))).read());
  }

  /**
   * Method under test: {@link ConcatResourceInputStream#read()}
   */
  @Test
  public void testRead4() throws IOException {
    // Arrange
    Project project = new Project();
    project.addDataTypeDefinition("ant.antunit.log", Object.class);
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertEquals(Retryable.RETRY_FOREVER,
        (new ConcatResourceInputStream(new LogContent(project, new LogLevel()))).read());
  }

  /**
   * Method under test: {@link ConcatResourceInputStream#read()}
   */
  @Test
  public void testRead5() throws IOException, BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget("ant.antunit.log", new Target());
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertEquals(Retryable.RETRY_FOREVER,
        (new ConcatResourceInputStream(new LogContent(project, new LogLevel()))).read());
  }
}


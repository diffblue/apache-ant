package org.apache.tools.ant.taskdefs.condition;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.listener.BigProjectLogger;
import org.junit.Test;

public class HttpDiffblueTest {
  /**
   * Test {@link Http#eval()}.
   * <ul>
   *   <li>Given {@link ByteArrayOutputStream#ByteArrayOutputStream(int)} with three.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Http#eval()}
   */
  @Test
  public void testEval_givenByteArrayOutputStreamWithThree_thenThrowBuildException() throws BuildException {
    // Arrange
    BigProjectLogger listener = new BigProjectLogger();
    listener.setOutputPrintStream(new PrintStream(new ByteArrayOutputStream(3)));

    Project project = new Project();
    project.addBuildListener(listener);

    Http http = new Http();
    http.setProject(project);
    http.setUrl("Checking for ");

    // Act and Assert
    assertThrows(BuildException.class, () -> http.eval());
  }

  /**
   * Test {@link Http#eval()}.
   * <ul>
   *   <li>Given {@link Http} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Http#eval()}
   */
  @Test
  public void testEval_givenHttpProjectIsProject_thenThrowBuildException() throws BuildException {
    // Arrange
    Http http = new Http();
    http.setProject(new Project());
    http.setUrl("Checking for ");

    // Act and Assert
    assertThrows(BuildException.class, () -> http.eval());
  }

  /**
   * Test {@link Http#eval()}.
   * <ul>
   *   <li>Given {@link Http} (default constructor) Url is {@code Checking for}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Http#eval()}
   */
  @Test
  public void testEval_givenHttpUrlIsCheckingFor_thenThrowBuildException() throws BuildException {
    // Arrange
    Http http = new Http();
    http.setUrl("Checking for ");

    // Act and Assert
    assertThrows(BuildException.class, () -> http.eval());
  }

  /**
   * Test {@link Http#eval()}.
   * <ul>
   *   <li>Given {@link Http} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Http#eval()}
   */
  @Test
  public void testEval_givenHttp_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Http()).eval());
  }

  /**
   * Test {@link Http#eval()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Http#eval()}
   */
  @Test
  public void testEval_givenJavaLangObject_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Badly formed URL: ", typeClass);
    project.addBuildListener(new AntClassLoader());

    Http http = new Http();
    http.setProject(project);
    http.setUrl("Checking for ");

    // Act and Assert
    assertThrows(BuildException.class, () -> http.eval());
  }

  /**
   * Test {@link Http#eval()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Http#eval()}
   */
  @Test
  public void testEval_givenProjectAddBuildListenerAntClassLoader_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Http http = new Http();
    http.setProject(project);
    http.setUrl("Checking for ");

    // Act and Assert
    assertThrows(BuildException.class, () -> http.eval());
  }

  /**
   * Test new {@link Http} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Http}
   */
  @Test
  public void testNewHttp() {
    // Arrange and Act
    Http actualHttp = new Http();

    // Assert
    Location location = actualHttp.getLocation();
    assertNull(location.getFileName());
    assertNull(actualHttp.getDescription());
    assertNull(actualHttp.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }
}

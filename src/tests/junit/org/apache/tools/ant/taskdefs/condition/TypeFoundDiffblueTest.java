package org.apache.tools.ant.taskdefs.condition;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.util.Map;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.ComponentHelper;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.junit.Test;

public class TypeFoundDiffblueTest {
  /**
   * Test {@link TypeFound#doesTypeExist(String)}.
   * <ul>
   *   <li>Given {@link TypeFound} (default constructor) URI is {@code foo}.</li>
   *   <li>Then {@link TypeFound} (default constructor) Project BuildListeners Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link TypeFound#doesTypeExist(String)}
   */
  @Test
  public void testDoesTypeExist_givenTypeFoundUriIsFoo_thenTypeFoundProjectBuildListenersEmpty() {
    // Arrange
    TypeFound typeFound = new TypeFound();
    typeFound.setProject(new Project());
    typeFound.setURI("foo");

    // Act
    typeFound.doesTypeExist("Typename");

    // Assert that nothing has changed
    assertTrue(typeFound.getProject().getBuildListeners().isEmpty());
  }

  /**
   * Test {@link TypeFound#eval()}.
   * <ul>
   *   <li>Given {@link TypeFound} (default constructor) Name is {@code Name}.</li>
   *   <li>Then {@link TypeFound} (default constructor) Project BuildListeners Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link TypeFound#eval()}
   */
  @Test
  public void testEval_givenTypeFoundNameIsName_thenTypeFoundProjectBuildListenersEmpty() throws BuildException {
    // Arrange
    TypeFound typeFound = new TypeFound();
    typeFound.setName("Name");
    typeFound.setProject(new Project());
    typeFound.setURI("Uri");

    // Act
    typeFound.eval();

    // Assert that nothing has changed
    assertTrue(typeFound.getProject().getBuildListeners().isEmpty());
  }

  /**
   * Test {@link TypeFound#eval()}.
   * <ul>
   *   <li>Given {@link TypeFound} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TypeFound#eval()}
   */
  @Test
  public void testEval_givenTypeFound_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new TypeFound()).eval());
  }

  /**
   * Test {@link TypeFound#eval()}.
   * <ul>
   *   <li>Then {@link TypeFound} (default constructor) Project CopyOfReferences size is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link TypeFound#eval()}
   */
  @Test
  public void testEval_thenTypeFoundProjectCopyOfReferencesSizeIsTwo() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addReference("ant.PropertyHelper", "Value");

    TypeFound typeFound = new TypeFound();
    typeFound.setName("Name");
    typeFound.setProject(project);
    typeFound.setURI("Uri");

    // Act
    typeFound.eval();

    // Assert that nothing has changed
    Project project2 = typeFound.getProject();
    Map<String, Object> copyOfReferences = project2.getCopyOfReferences();
    assertEquals(2, copyOfReferences.size());
    Object getResult = copyOfReferences.get("ant.ComponentHelper");
    assertTrue(getResult instanceof ComponentHelper);
    assertTrue(((ComponentHelper) getResult).getAntTypeTable().isEmpty());
    assertTrue(copyOfReferences.containsKey("ant.PropertyHelper"));
    assertTrue(project2.getBuildListeners().isEmpty());
  }

  /**
   * Test new {@link TypeFound} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link TypeFound}
   */
  @Test
  public void testNewTypeFound() {
    // Arrange and Act
    TypeFound actualTypeFound = new TypeFound();

    // Assert
    Location location = actualTypeFound.getLocation();
    assertNull(location.getFileName());
    assertNull(actualTypeFound.getDescription());
    assertNull(actualTypeFound.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }
}

package org.apache.tools.ant.taskdefs.condition;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.Reference;
import org.junit.Test;

public class IsReferenceDiffblueTest {
  /**
   * Test {@link IsReference#eval()}.
   * <ul>
   *   <li>Given {@link IsReference} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IsReference#eval()}
   */
  @Test
  public void testEval_givenIsReferenceProjectIsProject_thenReturnFalse() throws BuildException {
    // Arrange
    IsReference isReference = new IsReference();
    isReference.setProject(new Project());
    isReference.setRefid(new Reference("42"));

    // Act and Assert
    assertFalse(isReference.eval());
  }

  /**
   * Test {@link IsReference#eval()}.
   * <ul>
   *   <li>Given {@link IsReference} (default constructor) Type is {@code Type}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IsReference#eval()}
   */
  @Test
  public void testEval_givenIsReferenceTypeIsType_thenReturnFalse() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addReference("42", "Value");

    IsReference isReference = new IsReference();
    isReference.setType("Type");
    isReference.setProject(project);
    isReference.setRefid(new Reference("42"));

    // Act and Assert
    assertFalse(isReference.eval());
  }

  /**
   * Test {@link IsReference#eval()}.
   * <ul>
   *   <li>Given {@link IsReference} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IsReference#eval()}
   */
  @Test
  public void testEval_givenIsReference_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new IsReference()).eval());
  }

  /**
   * Test {@link IsReference#eval()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IsReference#eval()}
   */
  @Test
  public void testEval_givenJavaLangObject_thenReturnFalse() throws BuildException {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("ant.ComponentHelper", typeClass);
    project.addReference("42", "Value");

    IsReference isReference = new IsReference();
    isReference.setType("Type");
    isReference.setProject(project);
    isReference.setRefid(new Reference("42"));

    // Act and Assert
    assertFalse(isReference.eval());
  }

  /**
   * Test {@link IsReference#eval()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IsReference#eval()}
   */
  @Test
  public void testEval_givenProjectAddBuildListenerAntClassLoader_thenReturnFalse() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());
    project.addReference("42", "Value");

    IsReference isReference = new IsReference();
    isReference.setType("Type");
    isReference.setProject(project);
    isReference.setRefid(new Reference("42"));

    // Act and Assert
    assertFalse(isReference.eval());
  }

  /**
   * Test {@link IsReference#eval()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addReference {@code 42} and {@code Value}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IsReference#eval()}
   */
  @Test
  public void testEval_givenProjectAddReference42AndValue_thenReturnTrue() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addReference("42", "Value");

    IsReference isReference = new IsReference();
    isReference.setProject(project);
    isReference.setRefid(new Reference("42"));

    // Act and Assert
    assertTrue(isReference.eval());
  }

  /**
   * Test new {@link IsReference} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link IsReference}
   */
  @Test
  public void testNewIsReference() {
    // Arrange and Act
    IsReference actualIsReference = new IsReference();

    // Assert
    Location location = actualIsReference.getLocation();
    assertNull(location.getFileName());
    assertNull(actualIsReference.getDescription());
    assertNull(actualIsReference.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }
}

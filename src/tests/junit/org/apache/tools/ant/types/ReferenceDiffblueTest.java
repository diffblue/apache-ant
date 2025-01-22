package org.apache.tools.ant.types;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Target;
import org.junit.Test;

public class ReferenceDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Reference#Reference()}
   *   <li>{@link Reference#setProject(Project)}
   *   <li>{@link Reference#setRefId(String)}
   *   <li>{@link Reference#getProject()}
   *   <li>{@link Reference#getRefId()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    Reference actualReference = new Reference();
    Project p = new Project();
    actualReference.setProject(p);
    actualReference.setRefId("42");
    Project actualProject = actualReference.getProject();

    // Assert
    assertEquals("42", actualReference.getRefId());
    assertSame(p, actualProject);
  }

  /**
   * Test {@link Reference#Reference(String)}.
   * <p>
   * Method under test: {@link Reference#Reference(String)}
   */
  @Test
  public void testNewReference() {
    // Arrange and Act
    Reference actualReference = new Reference("42");

    // Assert
    assertEquals("42", actualReference.getRefId());
    assertNull(actualReference.getProject());
  }

  /**
   * Test {@link Reference#Reference(Project, String)}.
   * <p>
   * Method under test: {@link Reference#Reference(Project, String)}
   */
  @Test
  public void testNewReference2() {
    // Arrange
    Project p = new Project();

    // Act
    Reference actualReference = new Reference(p, "42");

    // Assert
    assertEquals("42", actualReference.getRefId());
    assertSame(p, actualReference.getProject());
  }

  /**
   * Test {@link Reference#getReferencedObject()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Reference#getReferencedObject()}
   */
  @Test
  public void testGetReferencedObject_givenJavaLangObject_thenThrowBuildException() throws BuildException {
    // Arrange
    Project p = new Project();
    Class<Object> typeClass = Object.class;
    p.addDataTypeDefinition("Adding reference: ", typeClass);

    Reference reference = new Reference("42");
    reference.setProject(p);
    reference.setRefId("");

    // Act and Assert
    assertThrows(BuildException.class, () -> reference.getReferencedObject());
  }

  /**
   * Test {@link Reference#getReferencedObject()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Reference#getReferencedObject()}
   */
  @Test
  public void testGetReferencedObject_givenProjectAddBuildListenerAntClassLoader() throws BuildException {
    // Arrange
    Project p = new Project();
    p.addBuildListener(new AntClassLoader());

    Reference reference = new Reference("42");
    reference.setProject(p);
    reference.setRefId("");

    // Act and Assert
    assertThrows(BuildException.class, () -> reference.getReferencedObject());
  }

  /**
   * Test {@link Reference#getReferencedObject()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addReference {@code ant.PropertyHelper} and {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Reference#getReferencedObject()}
   */
  @Test
  public void testGetReferencedObject_givenProjectAddReferenceAntPropertyHelperAndValue() throws BuildException {
    // Arrange
    Project p = new Project();
    p.addReference("ant.PropertyHelper", "Value");
    Class<Object> typeClass = Object.class;
    p.addDataTypeDefinition("Adding reference: ", typeClass);

    Reference reference = new Reference("42");
    reference.setProject(p);
    reference.setRefId("");

    // Act and Assert
    assertThrows(BuildException.class, () -> reference.getReferencedObject());
  }

  /**
   * Test {@link Reference#getReferencedObject()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code Adding reference:} and {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Reference#getReferencedObject()}
   */
  @Test
  public void testGetReferencedObject_givenProjectAddTargetAddingReferenceAndTarget() throws BuildException {
    // Arrange
    Project p = new Project();
    p.addTarget("Adding reference: ", new Target());

    Reference reference = new Reference("42");
    reference.setProject(p);
    reference.setRefId("");

    // Act and Assert
    assertThrows(BuildException.class, () -> reference.getReferencedObject());
  }

  /**
   * Test {@link Reference#getReferencedObject()}.
   * <ul>
   *   <li>Given {@link Reference#Reference(String)} with id is {@code 42} RefId is {@code ant.PropertyHelper}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Reference#getReferencedObject()}
   */
  @Test
  public void testGetReferencedObject_givenReferenceWithIdIs42RefIdIsAntPropertyHelper() throws BuildException {
    // Arrange
    Reference reference = new Reference("42");
    reference.setProject(new Project());
    reference.setRefId("ant.PropertyHelper");

    // Act and Assert
    assertThrows(BuildException.class, () -> reference.getReferencedObject());
  }

  /**
   * Test {@link Reference#getReferencedObject()}.
   * <ul>
   *   <li>Given {@link Reference#Reference(String)} with id is {@code 42} RefId is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Reference#getReferencedObject()}
   */
  @Test
  public void testGetReferencedObject_givenReferenceWithIdIs42RefIdIsEmptyString() throws BuildException {
    // Arrange
    Reference reference = new Reference("42");
    reference.setProject(new Project());
    reference.setRefId("");

    // Act and Assert
    assertThrows(BuildException.class, () -> reference.getReferencedObject());
  }

  /**
   * Test {@link Reference#getReferencedObject()}.
   * <ul>
   *   <li>Given {@link Reference#Reference(String)} with id is {@code 42} RefId is {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Reference#getReferencedObject()}
   */
  @Test
  public void testGetReferencedObject_givenReferenceWithIdIs42RefIdIsFoo() throws BuildException {
    // Arrange
    Reference reference = new Reference("42");
    reference.setProject(new Project());
    reference.setRefId("foo");

    // Act and Assert
    assertThrows(BuildException.class, () -> reference.getReferencedObject());
  }

  /**
   * Test {@link Reference#getReferencedObject()}.
   * <ul>
   *   <li>Given {@link Reference#Reference(String)} with id is {@code 42} RefId is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Reference#getReferencedObject()}
   */
  @Test
  public void testGetReferencedObject_givenReferenceWithIdIs42RefIdIsNull() throws BuildException {
    // Arrange
    Reference reference = new Reference("42");
    reference.setProject(new Project());
    reference.setRefId(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> reference.getReferencedObject());
  }

  /**
   * Test {@link Reference#getReferencedObject()}.
   * <ul>
   *   <li>Given {@link Reference#Reference(String)} with id is {@code 42}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Reference#getReferencedObject()}
   */
  @Test
  public void testGetReferencedObject_givenReferenceWithIdIs42_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Reference("42")).getReferencedObject());
  }

  /**
   * Test {@link Reference#getReferencedObject()}.
   * <ul>
   *   <li>Then return {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Reference#getReferencedObject()}
   */
  @Test
  public void testGetReferencedObject_thenReturnValue() throws BuildException {
    // Arrange
    Project p = new Project();
    p.addReference("", "Value");

    Reference reference = new Reference("42");
    reference.setProject(p);
    reference.setRefId("");

    // Act and Assert
    assertEquals("Value", reference.getReferencedObject());
  }
}

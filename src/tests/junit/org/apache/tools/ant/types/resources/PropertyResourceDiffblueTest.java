package org.apache.tools.ant.types.resources;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Target;
import org.junit.Test;

public class PropertyResourceDiffblueTest {
  /**
   * Test {@link PropertyResource#getValue()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyResource#getValue()}
   */
  @Test
  public void testGetValue_givenJavaLangObject() {
    // Arrange
    Project p = new Project();
    Class<Object> typeClass = Object.class;
    p.addDataTypeDefinition("Adding reference: ", typeClass);
    p.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertNull((new PropertyResource(p, "foo")).getValue());
  }

  /**
   * Test {@link PropertyResource#getValue()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyResource#getValue()}
   */
  @Test
  public void testGetValue_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project p = new Project();
    p.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertNull((new PropertyResource(p, "foo")).getValue());
  }

  /**
   * Test {@link PropertyResource#getValue()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code Adding reference:} and {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyResource#getValue()}
   */
  @Test
  public void testGetValue_givenProjectAddTargetAddingReferenceAndTarget() throws BuildException {
    // Arrange
    Project p = new Project();
    p.addTarget("Adding reference: ", new Target());
    p.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertNull((new PropertyResource(p, "foo")).getValue());
  }

  /**
   * Test {@link PropertyResource#getValue()}.
   * <ul>
   *   <li>Given {@link PropertyResource#PropertyResource()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyResource#getValue()}
   */
  @Test
  public void testGetValue_givenPropertyResource() {
    // Arrange, Act and Assert
    assertNull((new PropertyResource()).getValue());
  }

  /**
   * Test {@link PropertyResource#getValue()}.
   * <ul>
   *   <li>Given {@link PropertyResource#PropertyResource()} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyResource#getValue()}
   */
  @Test
  public void testGetValue_givenPropertyResourceProjectIsProject() {
    // Arrange
    PropertyResource propertyResource = new PropertyResource();
    propertyResource.setProject(new Project());

    // Act and Assert
    assertNull(propertyResource.getValue());
  }

  /**
   * Test {@link PropertyResource#getValue()}.
   * <ul>
   *   <li>Given {@link PropertyResource#PropertyResource(Project, String)} with p is {@link Project} (default constructor) and n is {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyResource#getValue()}
   */
  @Test
  public void testGetValue_givenPropertyResourceWithPIsProjectAndNIsFoo() {
    // Arrange, Act and Assert
    assertNull((new PropertyResource(new Project(), "foo")).getValue());
  }

  /**
   * Test {@link PropertyResource#getObjectValue()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyResource#getObjectValue()}
   */
  @Test
  public void testGetObjectValue_givenJavaLangObject() {
    // Arrange
    Project p = new Project();
    Class<Object> typeClass = Object.class;
    p.addDataTypeDefinition("Adding reference: ", typeClass);
    p.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertNull((new PropertyResource(p, "foo")).getObjectValue());
  }

  /**
   * Test {@link PropertyResource#getObjectValue()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyResource#getObjectValue()}
   */
  @Test
  public void testGetObjectValue_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project p = new Project();
    p.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertNull((new PropertyResource(p, "foo")).getObjectValue());
  }

  /**
   * Test {@link PropertyResource#getObjectValue()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code Adding reference:} and {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyResource#getObjectValue()}
   */
  @Test
  public void testGetObjectValue_givenProjectAddTargetAddingReferenceAndTarget() throws BuildException {
    // Arrange
    Project p = new Project();
    p.addTarget("Adding reference: ", new Target());
    p.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertNull((new PropertyResource(p, "foo")).getObjectValue());
  }

  /**
   * Test {@link PropertyResource#getObjectValue()}.
   * <ul>
   *   <li>Given {@link PropertyResource#PropertyResource()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyResource#getObjectValue()}
   */
  @Test
  public void testGetObjectValue_givenPropertyResource() {
    // Arrange, Act and Assert
    assertNull((new PropertyResource()).getObjectValue());
  }

  /**
   * Test {@link PropertyResource#getObjectValue()}.
   * <ul>
   *   <li>Given {@link PropertyResource#PropertyResource()} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyResource#getObjectValue()}
   */
  @Test
  public void testGetObjectValue_givenPropertyResourceProjectIsProject() {
    // Arrange
    PropertyResource propertyResource = new PropertyResource();
    propertyResource.setProject(new Project());

    // Act and Assert
    assertNull(propertyResource.getObjectValue());
  }

  /**
   * Test {@link PropertyResource#getObjectValue()}.
   * <ul>
   *   <li>Given {@link PropertyResource#PropertyResource(Project, String)} with p is {@link Project} (default constructor) and n is {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyResource#getObjectValue()}
   */
  @Test
  public void testGetObjectValue_givenPropertyResourceWithPIsProjectAndNIsFoo() {
    // Arrange, Act and Assert
    assertNull((new PropertyResource(new Project(), "foo")).getObjectValue());
  }

  /**
   * Test {@link PropertyResource#isExists()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyResource#isExists()}
   */
  @Test
  public void testIsExists_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project p = new Project();
    p.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertFalse((new PropertyResource(p, "foo")).isExists());
  }

  /**
   * Test {@link PropertyResource#isExists()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code Adding reference:} and {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyResource#isExists()}
   */
  @Test
  public void testIsExists_givenProjectAddTargetAddingReferenceAndTarget() throws BuildException {
    // Arrange
    Project p = new Project();
    p.addTarget("Adding reference: ", new Target());
    p.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertFalse((new PropertyResource(p, "foo")).isExists());
  }

  /**
   * Test {@link PropertyResource#isExists()}.
   * <ul>
   *   <li>Given {@link PropertyResource#PropertyResource()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyResource#isExists()}
   */
  @Test
  public void testIsExists_givenPropertyResource() {
    // Arrange, Act and Assert
    assertFalse((new PropertyResource()).isExists());
  }

  /**
   * Test {@link PropertyResource#isExists()}.
   * <ul>
   *   <li>Given {@link PropertyResource#PropertyResource()} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyResource#isExists()}
   */
  @Test
  public void testIsExists_givenPropertyResourceProjectIsProject() {
    // Arrange
    PropertyResource propertyResource = new PropertyResource();
    propertyResource.setProject(new Project());

    // Act and Assert
    assertFalse(propertyResource.isExists());
  }

  /**
   * Test {@link PropertyResource#isExists()}.
   * <ul>
   *   <li>Given {@link PropertyResource#PropertyResource(Project, String)} with p is {@link Project} (default constructor) and n is {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyResource#isExists()}
   */
  @Test
  public void testIsExists_givenPropertyResourceWithPIsProjectAndNIsFoo() {
    // Arrange, Act and Assert
    assertFalse((new PropertyResource(new Project(), "foo")).isExists());
  }

  /**
   * Test {@link PropertyResource#getSize()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyResource#getSize()}
   */
  @Test
  public void testGetSize_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project p = new Project();
    p.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertEquals(0L, (new PropertyResource(p, "foo")).getSize());
  }

  /**
   * Test {@link PropertyResource#getSize()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code Adding reference:} and {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyResource#getSize()}
   */
  @Test
  public void testGetSize_givenProjectAddTargetAddingReferenceAndTarget() throws BuildException {
    // Arrange
    Project p = new Project();
    p.addTarget("Adding reference: ", new Target());
    p.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertEquals(0L, (new PropertyResource(p, "foo")).getSize());
  }

  /**
   * Test {@link PropertyResource#getSize()}.
   * <ul>
   *   <li>Given {@link PropertyResource#PropertyResource()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyResource#getSize()}
   */
  @Test
  public void testGetSize_givenPropertyResource() {
    // Arrange, Act and Assert
    assertEquals(0L, (new PropertyResource()).getSize());
  }

  /**
   * Test {@link PropertyResource#getSize()}.
   * <ul>
   *   <li>Given {@link PropertyResource#PropertyResource()} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyResource#getSize()}
   */
  @Test
  public void testGetSize_givenPropertyResourceProjectIsProject() {
    // Arrange
    PropertyResource propertyResource = new PropertyResource();
    propertyResource.setProject(new Project());

    // Act and Assert
    assertEquals(0L, propertyResource.getSize());
  }

  /**
   * Test {@link PropertyResource#getSize()}.
   * <ul>
   *   <li>Given {@link PropertyResource#PropertyResource(Project, String)} with p is {@link Project} (default constructor) and n is {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyResource#getSize()}
   */
  @Test
  public void testGetSize_givenPropertyResourceWithPIsProjectAndNIsFoo() {
    // Arrange, Act and Assert
    assertEquals(0L, (new PropertyResource(new Project(), "foo")).getSize());
  }

  /**
   * Test {@link PropertyResource#equals(Object)}, and {@link PropertyResource#hashCode()}.
   * <ul>
   *   <li>When other is same.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link PropertyResource#equals(Object)}
   *   <li>{@link PropertyResource#hashCode()}
   * </ul>
   */
  @Test
  public void testEqualsAndHashCode_whenOtherIsSame_thenReturnEqual() {
    // Arrange
    PropertyResource propertyResource = new PropertyResource();

    // Act and Assert
    assertEquals(propertyResource, propertyResource);
    int expectedHashCodeResult = propertyResource.hashCode();
    assertEquals(expectedHashCodeResult, propertyResource.hashCode());
  }

  /**
   * Test {@link PropertyResource#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyResource#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(new PropertyResource(), 1);
  }

  /**
   * Test {@link PropertyResource#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyResource#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual2() {
    // Arrange, Act and Assert
    assertNotEquals(new PropertyResource(new Project(), "foo"), 1);
  }

  /**
   * Test {@link PropertyResource#equals(Object)}.
   * <ul>
   *   <li>When other is {@code null}.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyResource#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsNull_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(new PropertyResource(), null);
  }

  /**
   * Test {@link PropertyResource#equals(Object)}.
   * <ul>
   *   <li>When other is wrong type.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyResource#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsWrongType_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(new PropertyResource(), "Different type to PropertyResource");
  }

  /**
   * Test {@link PropertyResource#toString()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyResource#toString()}
   */
  @Test
  public void testToString_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project p = new Project();
    p.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertNull((new PropertyResource(p, "foo")).toString());
  }

  /**
   * Test {@link PropertyResource#toString()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code Adding reference:} and {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyResource#toString()}
   */
  @Test
  public void testToString_givenProjectAddTargetAddingReferenceAndTarget() throws BuildException {
    // Arrange
    Project p = new Project();
    p.addTarget("Adding reference: ", new Target());
    p.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertNull((new PropertyResource(p, "foo")).toString());
  }

  /**
   * Test {@link PropertyResource#toString()}.
   * <ul>
   *   <li>Given {@link PropertyResource#PropertyResource()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyResource#toString()}
   */
  @Test
  public void testToString_givenPropertyResource() {
    // Arrange, Act and Assert
    assertNull((new PropertyResource()).toString());
  }

  /**
   * Test {@link PropertyResource#toString()}.
   * <ul>
   *   <li>Given {@link PropertyResource#PropertyResource()} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyResource#toString()}
   */
  @Test
  public void testToString_givenPropertyResourceProjectIsProject() {
    // Arrange
    PropertyResource propertyResource = new PropertyResource();
    propertyResource.setProject(new Project());

    // Act and Assert
    assertNull(propertyResource.toString());
  }

  /**
   * Test {@link PropertyResource#toString()}.
   * <ul>
   *   <li>Given {@link PropertyResource#PropertyResource(Project, String)} with p is {@link Project} (default constructor) and n is {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyResource#toString()}
   */
  @Test
  public void testToString_givenPropertyResourceWithPIsProjectAndNIsFoo() {
    // Arrange, Act and Assert
    assertNull((new PropertyResource(new Project(), "foo")).toString());
  }

  /**
   * Test {@link PropertyResource#isReferenceOrProxy()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyResource#isReferenceOrProxy()}
   */
  @Test
  public void testIsReferenceOrProxy_givenJavaLangObject() {
    // Arrange
    Project p = new Project();
    Class<Object> typeClass = Object.class;
    p.addDataTypeDefinition("Adding reference: ", typeClass);
    p.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertFalse((new PropertyResource(p, "foo")).isReferenceOrProxy());
  }

  /**
   * Test {@link PropertyResource#isReferenceOrProxy()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyResource#isReferenceOrProxy()}
   */
  @Test
  public void testIsReferenceOrProxy_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project p = new Project();
    p.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertFalse((new PropertyResource(p, "foo")).isReferenceOrProxy());
  }

  /**
   * Test {@link PropertyResource#isReferenceOrProxy()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code Adding reference:} and {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyResource#isReferenceOrProxy()}
   */
  @Test
  public void testIsReferenceOrProxy_givenProjectAddTargetAddingReferenceAndTarget() throws BuildException {
    // Arrange
    Project p = new Project();
    p.addTarget("Adding reference: ", new Target());
    p.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertFalse((new PropertyResource(p, "foo")).isReferenceOrProxy());
  }

  /**
   * Test {@link PropertyResource#isReferenceOrProxy()}.
   * <ul>
   *   <li>Given {@link PropertyResource#PropertyResource()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyResource#isReferenceOrProxy()}
   */
  @Test
  public void testIsReferenceOrProxy_givenPropertyResource() {
    // Arrange, Act and Assert
    assertFalse((new PropertyResource()).isReferenceOrProxy());
  }

  /**
   * Test {@link PropertyResource#isReferenceOrProxy()}.
   * <ul>
   *   <li>Given {@link PropertyResource#PropertyResource()} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyResource#isReferenceOrProxy()}
   */
  @Test
  public void testIsReferenceOrProxy_givenPropertyResourceProjectIsProject() {
    // Arrange
    PropertyResource propertyResource = new PropertyResource();
    propertyResource.setProject(new Project());

    // Act and Assert
    assertFalse(propertyResource.isReferenceOrProxy());
  }

  /**
   * Test {@link PropertyResource#isReferenceOrProxy()}.
   * <ul>
   *   <li>Given {@link PropertyResource#PropertyResource(Project, String)} with p is {@link Project} (default constructor) and n is {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyResource#isReferenceOrProxy()}
   */
  @Test
  public void testIsReferenceOrProxy_givenPropertyResourceWithPIsProjectAndNIsFoo() {
    // Arrange, Act and Assert
    assertFalse((new PropertyResource(new Project(), "foo")).isReferenceOrProxy());
  }

  /**
   * Test {@link PropertyResource#getReferencedOrProxied()}.
   * <p>
   * Method under test: {@link PropertyResource#getReferencedOrProxied()}
   */
  @Test
  public void testGetReferencedOrProxied() {
    // Arrange, Act and Assert
    assertThrows(IllegalStateException.class, () -> (new PropertyResource(new Project(),
        "This PropertyResource does not reference or proxy another Resource")).getReferencedOrProxied());
  }

  /**
   * Test {@link PropertyResource#getReferencedOrProxied()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyResource#getReferencedOrProxied()}
   */
  @Test
  public void testGetReferencedOrProxied_givenJavaLangObject() {
    // Arrange
    Project p = new Project();
    Class<Object> typeClass = Object.class;
    p.addDataTypeDefinition("Adding reference: ", typeClass);
    p.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(IllegalStateException.class,
        () -> (new PropertyResource(p, "This PropertyResource does not reference or proxy another Resource"))
            .getReferencedOrProxied());
  }

  /**
   * Test {@link PropertyResource#getReferencedOrProxied()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyResource#getReferencedOrProxied()}
   */
  @Test
  public void testGetReferencedOrProxied_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project p = new Project();
    p.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(IllegalStateException.class,
        () -> (new PropertyResource(p, "This PropertyResource does not reference or proxy another Resource"))
            .getReferencedOrProxied());
  }

  /**
   * Test {@link PropertyResource#getReferencedOrProxied()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code Adding reference:} and {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyResource#getReferencedOrProxied()}
   */
  @Test
  public void testGetReferencedOrProxied_givenProjectAddTargetAddingReferenceAndTarget() throws BuildException {
    // Arrange
    Project p = new Project();
    p.addTarget("Adding reference: ", new Target());
    p.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(IllegalStateException.class,
        () -> (new PropertyResource(p, "This PropertyResource does not reference or proxy another Resource"))
            .getReferencedOrProxied());
  }

  /**
   * Test {@link PropertyResource#getReferencedOrProxied()}.
   * <ul>
   *   <li>Given {@link PropertyResource#PropertyResource()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyResource#getReferencedOrProxied()}
   */
  @Test
  public void testGetReferencedOrProxied_givenPropertyResource() {
    // Arrange, Act and Assert
    assertThrows(IllegalStateException.class, () -> (new PropertyResource()).getReferencedOrProxied());
  }

  /**
   * Test {@link PropertyResource#getReferencedOrProxied()}.
   * <ul>
   *   <li>Given {@link PropertyResource#PropertyResource()} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyResource#getReferencedOrProxied()}
   */
  @Test
  public void testGetReferencedOrProxied_givenPropertyResourceProjectIsProject() {
    // Arrange
    PropertyResource propertyResource = new PropertyResource();
    propertyResource.setProject(new Project());

    // Act and Assert
    assertThrows(IllegalStateException.class, () -> propertyResource.getReferencedOrProxied());
  }
}

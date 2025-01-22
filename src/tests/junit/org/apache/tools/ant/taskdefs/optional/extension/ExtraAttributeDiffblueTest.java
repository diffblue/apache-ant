package org.apache.tools.ant.taskdefs.optional.extension;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class ExtraAttributeDiffblueTest {
  /**
   * Test {@link ExtraAttribute#validate()}.
   * <ul>
   *   <li>Given {@link ExtraAttribute} (default constructor) Value is {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtraAttribute#validate()}
   */
  @Test
  public void testValidate_givenExtraAttributeValueIsNull_thenThrowBuildException() throws BuildException {
    // Arrange
    ExtraAttribute extraAttribute = new ExtraAttribute();
    extraAttribute.setName("foo");
    extraAttribute.setValue(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> extraAttribute.validate());
  }

  /**
   * Test {@link ExtraAttribute#validate()}.
   * <ul>
   *   <li>Given {@link ExtraAttribute} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtraAttribute#validate()}
   */
  @Test
  public void testValidate_givenExtraAttribute_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new ExtraAttribute()).validate());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link ExtraAttribute}
   *   <li>{@link ExtraAttribute#setName(String)}
   *   <li>{@link ExtraAttribute#setValue(String)}
   *   <li>{@link ExtraAttribute#getName()}
   *   <li>{@link ExtraAttribute#getValue()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    ExtraAttribute actualExtraAttribute = new ExtraAttribute();
    actualExtraAttribute.setName("Name");
    actualExtraAttribute.setValue("42");
    String actualName = actualExtraAttribute.getName();

    // Assert
    assertEquals("42", actualExtraAttribute.getValue());
    assertEquals("Name", actualName);
  }
}

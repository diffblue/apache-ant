package org.apache.tools.ant.types;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.junit.Test;

public class SubstitutionDiffblueTest {
  /**
   * Test new {@link Substitution} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Substitution}
   */
  @Test
  public void testNewSubstitution() {
    // Arrange and Act
    Substitution actualSubstitution = new Substitution();

    // Assert
    assertEquals("Substitution", actualSubstitution.getDataTypeName());
    Location location = actualSubstitution.getLocation();
    assertNull(location.getFileName());
    assertNull(actualSubstitution.getDescription());
    assertNull(actualSubstitution.getProject());
    assertNull(actualSubstitution.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualSubstitution.isReference());
    assertTrue(actualSubstitution.isChecked());
  }

  /**
   * Test {@link Substitution#getExpression(Project)}.
   * <p>
   * Method under test: {@link Substitution#getExpression(Project)}
   */
  @Test
  public void testGetExpression() {
    // Arrange
    Substitution substitution = new Substitution();

    // Act and Assert
    assertNull(substitution.getExpression(new Project()));
  }
}

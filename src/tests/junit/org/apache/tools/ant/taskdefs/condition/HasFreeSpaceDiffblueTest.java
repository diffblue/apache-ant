package org.apache.tools.ant.taskdefs.condition;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class HasFreeSpaceDiffblueTest {
  /**
   * Test {@link HasFreeSpace#eval()}.
   * <ul>
   *   <li>Given {@link HasFreeSpace} (default constructor) Needed is {@code 1.6}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link HasFreeSpace#eval()}
   */
  @Test
  public void testEval_givenHasFreeSpaceNeededIs16_thenThrowBuildException() throws BuildException {
    // Arrange
    HasFreeSpace hasFreeSpace = new HasFreeSpace();
    hasFreeSpace.setPartition("foo");
    hasFreeSpace.setNeeded("1.6");

    // Act and Assert
    assertThrows(BuildException.class, () -> hasFreeSpace.eval());
  }

  /**
   * Test {@link HasFreeSpace#eval()}.
   * <ul>
   *   <li>Given {@link HasFreeSpace} (default constructor) Needed is {@code 42}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link HasFreeSpace#eval()}
   */
  @Test
  public void testEval_givenHasFreeSpaceNeededIs42_thenReturnFalse() throws BuildException {
    // Arrange
    HasFreeSpace hasFreeSpace = new HasFreeSpace();
    hasFreeSpace.setPartition("foo");
    hasFreeSpace.setNeeded("42");

    // Act and Assert
    assertFalse(hasFreeSpace.eval());
  }

  /**
   * Test {@link HasFreeSpace#eval()}.
   * <ul>
   *   <li>Given {@link HasFreeSpace} (default constructor) Needed is {@code foo}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link HasFreeSpace#eval()}
   */
  @Test
  public void testEval_givenHasFreeSpaceNeededIsFoo_thenThrowBuildException() throws BuildException {
    // Arrange
    HasFreeSpace hasFreeSpace = new HasFreeSpace();
    hasFreeSpace.setPartition("foo");
    hasFreeSpace.setNeeded("foo");

    // Act and Assert
    assertThrows(BuildException.class, () -> hasFreeSpace.eval());
  }

  /**
   * Test {@link HasFreeSpace#eval()}.
   * <ul>
   *   <li>Given {@link HasFreeSpace} (default constructor) Needed is {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link HasFreeSpace#eval()}
   */
  @Test
  public void testEval_givenHasFreeSpaceNeededIsNull_thenThrowBuildException() throws BuildException {
    // Arrange
    HasFreeSpace hasFreeSpace = new HasFreeSpace();
    hasFreeSpace.setPartition("foo");
    hasFreeSpace.setNeeded(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> hasFreeSpace.eval());
  }

  /**
   * Test {@link HasFreeSpace#eval()}.
   * <ul>
   *   <li>Given {@link HasFreeSpace} (default constructor) Partition is {@code .}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link HasFreeSpace#eval()}
   */
  @Test
  public void testEval_givenHasFreeSpacePartitionIsDot_thenReturnTrue() throws BuildException {
    // Arrange
    HasFreeSpace hasFreeSpace = new HasFreeSpace();
    hasFreeSpace.setPartition(".");
    hasFreeSpace.setNeeded("42");

    // Act and Assert
    assertTrue(hasFreeSpace.eval());
  }

  /**
   * Test {@link HasFreeSpace#eval()}.
   * <ul>
   *   <li>Given {@link HasFreeSpace} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link HasFreeSpace#eval()}
   */
  @Test
  public void testEval_givenHasFreeSpace_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new HasFreeSpace()).eval());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link HasFreeSpace}
   *   <li>{@link HasFreeSpace#setNeeded(String)}
   *   <li>{@link HasFreeSpace#setPartition(String)}
   *   <li>{@link HasFreeSpace#getNeeded()}
   *   <li>{@link HasFreeSpace#getPartition()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    HasFreeSpace actualHasFreeSpace = new HasFreeSpace();
    actualHasFreeSpace.setNeeded("Needed");
    actualHasFreeSpace.setPartition("Partition");
    String actualNeeded = actualHasFreeSpace.getNeeded();

    // Assert
    assertEquals("Needed", actualNeeded);
    assertEquals("Partition", actualHasFreeSpace.getPartition());
  }
}

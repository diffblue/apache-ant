package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class ProcessDestroyerDiffblueTest {
  /**
   * Test new {@link ProcessDestroyer} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link ProcessDestroyer}
   */
  @Test
  public void testNewProcessDestroyer() {
    // Arrange, Act and Assert
    assertFalse((new ProcessDestroyer()).isAddedAsShutdownHook());
  }

  /**
   * Test {@link ProcessDestroyer#isAddedAsShutdownHook()}.
   * <p>
   * Method under test: {@link ProcessDestroyer#isAddedAsShutdownHook()}
   */
  @Test
  public void testIsAddedAsShutdownHook() {
    // Arrange, Act and Assert
    assertFalse((new ProcessDestroyer()).isAddedAsShutdownHook());
  }

  /**
   * Test {@link ProcessDestroyer#add(Process)}.
   * <p>
   * Method under test: {@link ProcessDestroyer#add(Process)}
   */
  @Test
  public void testAdd() {
    // Arrange
    ProcessDestroyer processDestroyer = new ProcessDestroyer();

    // Act and Assert
    assertTrue(processDestroyer.add(null));
    assertTrue(processDestroyer.isAddedAsShutdownHook());
  }

  /**
   * Test {@link ProcessDestroyer#remove(Process)}.
   * <p>
   * Method under test: {@link ProcessDestroyer#remove(Process)}
   */
  @Test
  public void testRemove() {
    // Arrange, Act and Assert
    assertFalse((new ProcessDestroyer()).remove(null));
  }
}

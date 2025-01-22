package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import org.apache.tools.ant.util.WeakishReference.HardReference;
import org.junit.Test;

public class WeakishReferenceDiffblueTest {
  /**
   * Test HardReference {@link HardReference#HardReference(Object)}.
   * <p>
   * Method under test: {@link HardReference#HardReference(Object)}
   */
  @Test
  public void testHardReferenceNewHardReference() {
    // Arrange, Act and Assert
    assertEquals("Object", (new HardReference("Object")).get());
  }

  /**
   * Test {@link WeakishReference#WeakishReference(Object)}.
   * <p>
   * Method under test: {@link WeakishReference#WeakishReference(Object)}
   */
  @Test
  public void testNewWeakishReference() {
    // Arrange, Act and Assert
    assertEquals("Reference", (new WeakishReference("Reference")).get());
  }

  /**
   * Test {@link WeakishReference#get()}.
   * <p>
   * Method under test: {@link WeakishReference#get()}
   */
  @Test
  public void testGet() {
    // Arrange, Act and Assert
    assertEquals("Object", WeakishReference.createReference("Object").get());
  }

  /**
   * Test {@link WeakishReference#createReference(Object)}.
   * <p>
   * Method under test: {@link WeakishReference#createReference(Object)}
   */
  @Test
  public void testCreateReference() {
    // Arrange, Act and Assert
    assertEquals("Object", WeakishReference.createReference("Object").get());
  }
}
